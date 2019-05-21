#########################################################

# parameters

#intrinsic rate of increase 
lambda <- 10
#intraspecific competition
alpha <- .1
#distance decay rate (low mdd)
m1 <- 1
#distance decay rate (high mdd)
m2<-0.8
#starting population (small m)
N_0 <- 10
#starting population (big m)
M_0<-2
#number of generations to simulate 
g_max <- 10
#number of patches
patch <- 50
#distance between patches vector
dpatch <- 1
#Define distance vector (for now must be integer distances between patches)
dvect <- seq(from=0,length.out = patch,by=dpatch)

##########################################################

#functions

#seed production function (now discrete)
gNty <- function(j){
  seeds<-vector()
  jnext<-(lambda*j)/(1+alpha*j)
  seeds<-c(seeds,jnext)
  return(round(seeds,0))
}

#Stochastic Dispersal function TESTED AND IT WORKS :D
#right now this can only cope with integer distances. I think I can define buckets of each patch 
#if there are looser bounds ie some defined interval that is considered "in the patch" then I can extend to 
#smaller distances between patches. The way this works now, a 1 unit distance between patches is continuous
#gaps are created with the distance vector.
disperse <- function(x,y){ #x is the number of seeds, y is the index of the starting patch
  pmove <- vector("numeric",patch)
  if (x==0) #need this to cope with NA
    return(pmove) #if there are no seeds, produce a vector of zeros
  else #if there are seeds, we disperse them
    distance<-round(rexp(x,m),0) #if I make "buckets" this can output a real value vs. integer
    direction<-rbinom(x,1,0.5) #this decided if seeds move forwards or back
    for (i in 1:x){ #this refactors direction to 1,-1
     if (direction[i]==0)
        direction[i]<- -1
   }
   move<-distance*direction #this combines distance and direction
   for (i in 1:x){ #this moves seeds to their new home, x is number of seeds, compute for each seed 
     for (j in 1:patch){ #this finds the corresponding home for each seed 
        if (move[i]==pwdist[y,j]) #from the random output and pairwise distance matrix
          pmove[j]<-pmove[j]+1    #adds one to appropriate entry in vector if that is where a seed lands
      }
   }
   return(pmove) #vector of dispersed seeds
  
} #this might make sense to break into helper functions


#dispersal function second version -- refactored to remove unnecessary loops and seperate steps into helpers
disperse2<-function(x,y,m){
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
#dispersal helper -- generate random distances
distance<-function(x,m){
  dist<-round(rexp(x,m),0)
  return(dist)
}

#dispersal helper -- generate random directions
direction<-function(x){
  dir<-rbinom(x,1,0.5)
  sapply(dir,refactor)
}

#direction helper -- refactor 0 to -1
refactor<-function(x){
  if (x==0)
    x<- -1
  else x
}

##########################################################

# Initialize matrices for storing data

#Population Spread Matrix
#Adds values to each row for each generation
#Coloumns correspond to patches defined by dvect
spread<-matrix(0,g_max,patch)
spread[1,1]<-N_0
spreadhm<-matrix(0,g_max,patch)
spreadhm[1,1]<-M_0


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

#Speed vectors 
speedv1<-vector("numeric", g_max)
speedv2<-vector("numeric",g_max)

####################################################################

#Run Simulation

for (i in 1:(g_max-1)){ #for each generation
  total<-spread[i,]+spreadhm[i,]
  ratio<-spread[i,]/total
  ratio[is.nan(ratio)] <- 0
  seeds<-gNty(total)
  seedsp1<-round(ratio*seeds)
  seedsp2<-round((1-ratio)*seeds)
  for (j in 1:patch){ #disperse seeds
    seedsd1[j,]<-disperse2(seedsp1[j],j,m1)
    seedsd2[j,]<-disperse2(seedsp2[j],j,m2)
  }
  spread[i+1,]<-apply(seedsd1,2,sum) #fill in next row in population matrix
  spreadhm[i+1,]<-apply(seedsd2,2,sum)
  row1<-rev(spread[i+1,])
  row2<-rev(spreadhm[i+1,])
  speed1<-0
  speed2<-0
  for (k in 1:patch){
    if (row1[k] != 0){
      break
    }
    speed1<-speed1+1
  }
  for (k in 1:patch){
    if (row2[k] != 0){
      break
    }
    speed2<-speed2+1
  }
  col1[i]<-patch-speed1
  col2[i]<-patch-speed2
}
for (i in 1:g_max){
  if (i == 1){
   speedv1[i]<-col1[i]-1
  }
  else if (col1[i] == 0){
    speedv1[i]<-0
  }
 else
   speedv1[i]=col1[i]-col1[i-1]
 }
for (i in 1:g_max){
 if (i == 1){
   speedv2[i]<-col1[i]-1
 }
  else if (col2[i] == 0){
    speedv2[i]<-0
  }
 else
    speedv2[i]=col2[i]-col2[i-1]
}
