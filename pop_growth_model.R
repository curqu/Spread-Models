# model dispersal

#########################################################

# parameters

#intrinsic rate of increase 
lambda <- 10
#intraspecific competition
alpha <- .1
#distance decay rate
m <- 0.3
#starting population
N_0 <- 100
#number of generations to simulate
g_max <- 10
#number of patches
patch<-10
#distance between patches vector
dpatch<- 0.1
#Define distance vector
dvect<-seq(from=0,length.out = patch,by=dpatch)

##########################################################

#functions

#seed production function
#WHY DOES IT MAKE LESS SEEDS THAN POP? (Parameters, I think?)
gNty <- function(j){
  seeds<-vector()
    jnext<-(lambda*j)/(1+alpha*j)
    seeds<-c(seeds,jnext)
    return(seeds)
}

#dispersal kernel
ker <- function(x,y){
  k <-(m/2)*exp(-m*abs(x-y))
  return(k)
}
  
##########################################################

#Construct Probability Matrix for Dispersal

#Rows are starting patch
#Coloumns are colonized patch
#vector of probabilities
dprobv<-vector()
for (i in dvect){
  for (j in dvect){
    pd<-ker(i,j)
    dprobv<-c(dprobv,pd)
  }
}

#Convert to Matrix
dprob<-matrix(dprobv,nrow = patch, ncol = patch) 

##########################################################

#Initialize Population Matrix

#Rows are timesteps
#Coloumns are Patches -- Patch i corresponds to ith distance in dvect
N_i<-matrix(0,1,patch)
N_i[1,1]<-N_0

##########################################################

#Run for time t_i

#Set Current timestep
Nt_i<-tail(N_i,1)

#Produce Seeds
seedsv<-gNty(Nt_i)

#Disperse Seeds 
#match seedsv with originating row in dprob
disp<-vector()
for (i in 1:patch){
  for (j in 1:patch){
    dfromi<-seedsv[i]*dprob[i,j]
    disp<-c(disp,dfromi)
  }
}
#Convert to matrix
#coloumns correspond to originating patches
#rows are destination patch
disp<-matrix(disp,patch,patch)

#calculate dispersal for each entry
#sum total seeds
tdisp<-vector()
for (i in 1:patch){
  drt<-sum(disp[i,])
  tdisp<-c(tdisp,drt)
}

#populate next row of N_i
N_i<-rbind(N_i,tdisp)


#OLD WAY BUT WRONG WAY???
#Define Starting Matrix -- row 1 dist, 2 pop., 3 seeds
#N_i <- matrix(0,3,patch)
#Initialize starting pop
#N_i[2,1]<-N_0

#N_i[1,]<-dvect

#produce seeds

#for(j in N_i[2,]){
  # N_i[3,] <- gNty(N_i[2,])
# }

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

  
# for (i in y){
#   pd<-ker(1,i)
#   density<-c(density,pd)
# }
