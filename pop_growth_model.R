# model dispersal

# parameters

#intrinsic rate of increase 
lambda <- 10 #try different values
#intraspecific competition
alpha <- 1 #try different values
#distance decay rate
m <- 0.3 #try different values
#starting population
N_0 <- 30 #try different values
#number of generations to simulate
g_max <- 10 #unused so far, need to think about how to automate generations
#number of patches
patch<-10
#distance between patches vector
dpatch<- 0.1 #I think that this method should incorporate patchiness, since each distance cell is a kind of box?
#if I can figure out how to vary this across the landscape, could serve as a proxy for fuzziness? ie. if the distances vector is 
#(0.1, 0.2, 0.3, 1, 2, 3, 3.1, 3.2, 3.3)

#functions

#seed production function (from Papchepsky and Levine)
gNty <- function(j){
    jnext<-(lambda*j)/(1+alpha*j)
    return(jnext)
}

#dispersal kernel (from Pachepsky and Levine) 
#this might be off, probabilities adding up to more than 1?
ker <- function(x,y){
  k <-(m/2)*exp(-m*abs(x-y))
  return(k)
}

#Population density
Ntnextx <- #probably should delete this function


#Define Starting Matrix -- row 1 is distance vector, row 2 is population, row 3 is seeds produced (should reinitialize back to zero after each generation)
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

#tried to do it all in one step, but this is probably not a good idea, instead made probability matrix,
#need to define a function that will disperse seeds in [3,i] to cells [2,j] based on value in dprob[i,j]
#something like N_i[2,j]=N_i[3,i]*dprob[i,j]+N_i[2,j], looping somehow, how to record coloumn numbers in loop?
#try: change distance values back to patch #, use distance vector only to create dprob, or add row for patch#?

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

#move this to initialization section, dprob is constant
#change N_i[1,] to dvect? then N_i[1,] should be indices of patches [1:patch]
dprobv<-vector()
for (i in N_i[1,]){
  for (j in N_i[1,]){
    pd<-ker(i,j)
    dprobv<-c(dprobv,pd)
  }
}

dprob<-matrix(dprobv,nrow = patch, ncol = patch) 

  
