#########################################################

# parameters

#intrinsic rate of increase 
lambda <- 10
#intraspecific competition
alpha <- .1
#distance decay rate
m <- 0.3
#starting population
N_0 <- 10
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
disperse2<-function(x,y){
  pmove<-vector("numeric", patch)
  if (x==0)
    return(pmove)
  else
    move<-distance(x)*direction(x)
  for (i in 1:x){ #this moves seeds to their new home, x is number of seeds, compute for each seed 
    for (j in 1:patch){ #this finds the corresponding home for each seed 
      place(move[i],pwdist[y,j],pmove[j])
    }
  }
 return(pmove) 
}
#dispersal helper -- generate random distances
distance<-function(x){
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

#disperse helper - places seed in new patch based on dispersal distance
place<-function(x,y,z){
  if (x==y)
    z<-z+1
  else
    z
}

##########################################################

# Initialize matrices for storing data

#Population Spread Matrix
#Adds values to each row for each generation
#Coloumns correspond to patches defined by dvect
spread<-matrix(0,g_max,patch)
spread[1,1]<-N_0

#Seeds Dispersed
#vector for storing seeds produced, resets each generation
seedsp<-vector("numeric", patch)
#matrix containing number of plants/seeds in each patch after dispersal
#rows are originating patches where seeds were produced,
#coloumns are where they disperse to
seedsd<-matrix(0,patch,patch)

#Pairwise Distances Matrix
#Distances between patch i,j for row i 
#and coloumn j
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}

####################################################################

#Run Simulation

for (i in 1:(g_max-1)){ #for each generation
  seedsp<-gNty(spread[i,]) #make seeds
  for (j in 1:patch){ #disperse seeds
    seedsd[j,]<-disperse(seedsp[j],j)
  }
  spread[i+1,]<-apply(seedsd,2,sum) #fill in next row in population matrix
}

