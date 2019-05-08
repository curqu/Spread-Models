# model dispersal

# parameters

#intrinsic rate of increase 
lambda <- 10
#intraspecific competition
alpha <- 1
#distance decay rate
m <- 0.3
#starting population
N_0 <- 30
#number of generations to simulate
g_max <- 10
#number of patches
patch<-10
#distance between patches vector
dpatch<- 0.1
#functions

#seed production function
gNty <- function(j){
    jnext<-(lambda*j)/(1+alpha*j)
    return(jnext)
}

#dispersal kernel
ker <- function(x,y){
  k <-(m/2)*exp(-m*abs(x-y))
  return(k)
}

#Population density
Ntnextx <- 


#Define Starting Matrix -- row 1 dist, 2 pop., 3 seeds
N_i <- matrix(0,3,patch)
#Initialize starting pop
N_i[2,1]<-N_0
#Define distance vector
dvect<-seq(from=0,length.out = patch,by=dpatch)
N_i[1,]<-dvect

#produce seeds

for(j in N_i[2,]){
  N_i[3,] <- gNty(N_i[2,])
}

#disperse seeds

#for (i in col(N_i)){
#  for (j in N_i[1,]){
#     for (k in N_i[1,]){
#      N_i[3,]<-i*ker(j,k)
#     }
#  }
#}

#for (i in N_i[1,]){
#  for (j in N_i[1,]){
#    pd<-ker(i,j)
#    dprob[,] <- pd
#  }
#}
dprobv<-vector()
for (i in N_i[1,]){
  for (j in N_i[1,]){
    pd<-ker(i,j)
    dprobv<-c(dprobv,pd)
  }
}

dprob<-matrix(dprobv,nrow = patch, ncol = patch) 

  
