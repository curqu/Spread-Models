#########################################################

# parameters

#intrinsic rate of increase 
lambda <- 10
#intraspecific competition (competitive)
alpha1 <- .1
#intraspecific competition (less competitive)
alpha2<- .4
#distance decay rate (low mdd)
m1 <- 1.3
#distance decay rate (high mdd)
m2<-1
#starting population (competitive, bad dispersers)
N_0 <- 10
#starting population (less competitive, good dispersers)
M_0<-10
#number of generations to simulate 
g_max <- 30
#number of patches
patch <- 100
#distance between patches vector
dpatch <- 1
#Define distance vector (for now must be integer distances between patches)
dvect <- seq(from=0,length.out = patch,by=dpatch)

##########################################################

#functions

#seed production function
gNty <- function(j,k,alpha){ #j is subpopulation, k is total, alpha is competition
  seeds<-vector()
  jnext<-(lambda*j)/(1+alpha*(k-1))
  seeds<-c(seeds,jnext)
  return(round(seeds,0))
}

#Stochastic Dispersal function TESTED AND IT WORKS :D
#right now this can only cope with integer distances. I think I can define buckets of each patch 
#if there are looser bounds ie some defined interval that is considered "in the patch" then I can extend to 
#smaller distances between patches. The way this works now, a 1 unit distance between patches is continuous
#gaps are created with the distance vector.
# disperse <- function(x,y){ #x is the number of seeds, y is the index of the starting patch
#   pmove <- vector("numeric",patch)
#   if (x==0) #need this to cope with NA
#     return(pmove) #if there are no seeds, produce a vector of zeros
#   else #if there are seeds, we disperse them
#     distance<-round(rexp(x,m),0) #if I make "buckets" this can output a real value vs. integer
#     direction<-rbinom(x,1,0.5) #this decided if seeds move forwards or back
#     for (i in 1:x){ #this refactors direction to 1,-1
#      if (direction[i]==0)
#         direction[i]<- -1
#    }
#    move<-distance*direction #this combines distance and direction
#    for (i in 1:x){ #this moves seeds to their new home, x is number of seeds, compute for each seed 
#      for (j in 1:patch){ #this finds the corresponding home for each seed 
#         if (move[i]==pwdist[y,j]) #from the random output and pairwise distance matrix
#           pmove[j]<-pmove[j]+1    #adds one to appropriate entry in vector if that is where a seed lands
#       }
#    }
#    return(pmove) #vector of dispersed seeds
  
#} #this might make sense to break into helper functions


#dispersal function second version -- refactored to remove unnecessary loops and seperate steps into helpers
disperse<-function(x,y,m){ # x is the number of seeds in the patch
  # y is the coloumn index of the patch 
  # m is the parameter defining the dispersal kernel
  pmove<-vector("numeric", patch)
  if (x==0)
    return(pmove)
  else
    move<-distance(x,m)*direction(x)
  for (i in 1:x){ #this moves seeds to their new home, x is number of seeds, compute for each seed 
    for (j in 1:patch){ #this finds the corresponding home for each seed 
     # place(move[i],pwdist[y,j],pmove[j])
      if (move[i]==pwdist[y,j])
      {pmove[j]<-pmove[j]+1}
    }
  }
 return(pmove) 
}
#dispersal helper -- generate random distances, parameters are from disperse
distance<-function(x,m){
  dist<-round(rexp(x,m),0)
  return(dist)
}

#dispersal helper -- generate random directions,x is as disperse
direction<-function(x){
  dir<-rbinom(x,1,0.5)
  sapply(dir,refactor)
}

#direction helper -- refactor 0 to -1
refactor<-function(x){ #  x is values from binomial 
  if (x==0)
    x<- -1
  else x
}

#calculate how many new patches are colonized at the leading edge this generation
lemove<-function(x){ #x is the row in spread matrix corresponding to time t
  moved<-patch-pleft(rev(x))
  return(moved)
}

#lemove helper how many patches left to colonize
pleft<-function(x){ # x is from lemove, 
  y<-0
  for (k in 1:patch){
    if (x[k] != 0){
      break
    }
    else y<-y+1
  }
  return(y)
}

#calculate how many patches the leading edge moves each generation
# assumes that only one patch is colonized at start
speed<- function(x,y){ # x is vector of the leading edge patch each generation
  # y is the output vector, showing the number of patches the leading edge moved
  for (i in 1:g_max){
    if (i == 1){
      y[i]<-x[i]-1
   }
    else if (x[i] == 0){
     y[i]<-0
   }
    else
      y[i]=x[i]-x[i-1]
  }
  return(y)
}
##########################################################

# Initialize structures for storing data

#Population Spread Matrix
#Adds values to each row for each generation
#Coloumns correspond to patches defined by dvect
spreada<-matrix(0,g_max,patch)
spreada[1,1]<-N_0
spreadd<-matrix(0,g_max,patch)
spreadd[1,1]<-M_0
spreadttl<-matrix(0,g_max,patch)
spreadttl[1,]<-spreada[1,]+spreadd[1,]


#Seeds Dispersed
#vector for storing seeds produced, resets each generation
seedsp1<-vector("numeric", patch)
seedsp2<-vector("numeric",patch)
#matrix containing number of plants/seeds in each patch after dispersal
#rows are originating patches where seeds were produced,
#coloumns are where they disperse to
seedsd1<-matrix(0,patch,patch)
seedsd2<-matrix(0,patch,patch)

#Pairwise Distances Matrix
#Distances between patch i,j for row i 
#and coloumn j
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}

# vectors for storing colonized patches
col1<-vector("numeric", g_max)
col2<-vector("numeric", g_max)
colt<-vector("numeric", g_max)

#Speed vectors 
speedv1<-vector("numeric", g_max)
speedv2<-vector("numeric",g_max)
speedt<-vector("numeric", g_max)

# Average speed vectors
#speedmn1<-vector("numeric",nsim)
#speedmn2<-vector("numeric",nsim)
#speedmnt<-vector("numeric",nsim)

####################################################################

#Run Simulation

for (i in 1:(g_max-1)){ #for each generation
  total<-spreada[i,]+spreadd[i,]
  # ratio<-spread[i,]/total
  # ratio[is.nan(ratio)] <- 0
  seeds1<-gNty(spreada[i,],total,alpha1)
  seeds2<-gNty(spreadd[i,],total,alpha2)
  # seedsp1<-round(ratio*seeds)
  # seedsp2<-round((1-ratio)*seeds)
  for (j in 1:patch){ #disperse seeds
    seedsd1[j,]<-disperse(seeds1[j],j,m1)
    seedsd2[j,]<-disperse(seeds2[j],j,m2)
  }
  spreada[i+1,]<-apply(seedsd1,2,sum) #fill in next row in population matrix
  spreadd[i+1,]<-apply(seedsd2,2,sum)
  spreadttl[i+1,]<-spread[i+1,]+spreadhm[i+1,]
  col1[i]<-lemove(spreada[i+1,])#calculate how many patches the leading edge has advanced
  col2[i]<-lemove(spreadd[i+1,])
  colt[i]<-lemove(spreadttl[i+1,])
  
}
speedv1<-speed(col1,speedv1) #combine advances to get invasion velocity of leading edge
speedv2<-speed(col2,speedv2)
speedt<-speed(colt,speedt)
speedm1<-mean(speedv1) # average invasion velocity over time
speedm2<-mean(speedv2)
speedmt<-mean(speedt)