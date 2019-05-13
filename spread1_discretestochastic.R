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
#number of generations to simulate (I don't use this so far, need to figure out how to automate whole simulation)
g_max <- 10
#number of patches
patch<-10
#distance between patches vector
dpatch<- 1
#Define distance vector (for now must be integer distances between patches)
dvect<-seq(from=0,length.out = patch,by=dpatch)

##########################################################

#functions

#seed production function (now discrete)
gNty <- function(j){
  seeds<-vector()
  jnext<-(lambda*j)/(1+alpha*j)
  seeds<-c(seeds,jnext)
  return(round(seeds,0))
}

#dispersal kernel old 
# ker <- function(x,y){
#   k <-(m/2)*exp(-m*abs(x-y))
#   return(k)
}

#Stochastic Dispersal function
disperse <- function(x,y){
  pmove<-vector("numeric",patch)
  if (x==0)
    return(pmove)
  else
    distance<-round(rexp(x,m),0)
    direction<-rbinom(x,1,0.5)
    for (i in 1:x){
     if (direction[i]==0)
        direction[i]<- -1
   }
   move<-distance*direction
   for (i in 1:x){
     for (j in 1:patch){
        if (move[i]==pwdist[y,j])
          pmove[j]<-pmove[j]+1    
      }
   }
   return(pmove)
  
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

for (i in 1:(g_max-1)){
  seedsp<-gNty(spread[i,])
  # moves<-sapply(seedsp,disperse) 
  for (j in 1:patch){
    seedsd[j,]<-disperse(seedsp[j],j)
  }
  spread[i+1,]<-apply(seedsd,2,sum)
}
