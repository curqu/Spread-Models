#########################################################

# MASTER SCRIPT

# parameters

## THESE NEED TO BE UPDATED EACH MODEL RUN from parameter setup file -- can load into .Rdata first
#intrinsic rate of increase
lambda <- lambda.high
#landscape
dvect<-dvect_8

# THESE DO NOT CHANGE,keep static for all simulation sets
#distance decay rate of dispersal kernel
m<-m
#intraspecific competition
alpha <- .1
#number of strategies 
strat_t<-16
#starting population for each strategy
N_0 <- 3
#number of simulations to run
nsim<-1000
#number of generations to simulate 
g_max <- 40
#number of suitable patches
patch <- length(dvect)


##########################################################

#functions
# here all all the functions used in simulations, main functions are denoted by three pound signs,
# helper functions by two pounds.

### seed production function
# Beverton-Holt model of density dependent growth, modified to consider all individuals in a patch 
# as competitors, but only the relevant subpopulation for growth rate
# computed for each strategy population j across the landscape of n patches (vectorized)

gNty <- function(j,k,lambda){ 
  # j is number of individuals belonging to strategy j in patch x in 1:n, 
  # k is total population of all strategies in patch x in 1:n
  # lambda is reproductuve rate at low density of strategy j
  # first create a vector to store seeds 
  seeds<-vector()
  # compute Beverton-Holt population growth model
  seeds<-(lambda*j)/(1+alpha*(k-1))
  # round results to represent discrete individuals
  return(round(seeds,0))
}

### dispersal function 
# move seeds in patch x to patch y according to exponential dispersal kernel
# with random direction (forward & back)

disperse<-function(x,y,m){ 
  # x is the number of seeds in the patch to disperse
  # y is the row index of the patch in pair-wise distance matrix
  # m is the parameter defining the dispersal kernel
  # first define a vector to store dispersal distances
  pmove<-vector("numeric", patch)
  # if there are no seeds in patch x, return the empty vector
  if (x==0)
    return(pmove)
  # otherwise, compute distance and direction with helper functions below
  else
    move<-distance(x,m)*direction(x)
  for (i in 1:x){ 
    #this moves seeds to their new home, x is number of seeds, compute for each seed 
    for (j in 1:patch){ 
      #this finds the corresponding home for each seed 
      # place(move[i],pwdist[y,j],pmove[j])
      if (move[i]==pwdist[y,j])
      {pmove[j]<-pmove[j]+1}
    }
  }
  return(pmove) 
}

## helper function for disperse-- generate random distances, 
# parameters are from disperse function:
# x is the number of seeds in the patch
# m is the defined dispersal kernel parameter
distance<-function(x,m){
  # generate x random variates from exponental kernel with rate=m
  dist<-round(rexp(x,m),0)
  return(dist)
}

## helper function for disperse-- generate random directions,
# x is from disperse, number of seeds in patch
direction<-function(x){
  # draw x random variates from binomial distribution with p=0.5
  dir<-rbinom(x,1,0.5)
  # refactor results so that 0's return -1 to indicate negative direction
  # (disperse backwards/leftwards)
  sapply(dir,refactor)
}

## helper for direction-- refactor 0 to -1
refactor<-function(x){ 
  #  x is values from binomial distribution in direction
  if (x==0)
    x<- -1
  else x
}

### find the leading edge patch to record speed of advance

lemove<-function(x){ 
  # x is the row in spread matrix corresponding to time t
  # compute the 
  moved<-patch-pleft(rev(x))
  if (moved == 0){
    return(moved)
  }
  else{
    moved<-dvect[moved]
    return(moved) 
  }
  
}
#lemove helper how many patches left to colonize
pleft<-function(x){ 
  # x is from lemove, the row in spread matrix corresponding to time t
  # note we reverse the order so the first value in x corresponds to the right-most patch 
  # set index value y -- counts how far from the rightmost patch the leading edge is
  y<-0
  # for each patch, check if there is a non-zero population
  for (k in 1:patch){
    if (x[k] != 0){
      # halt once we reach the leading edge
      break
    }
    # otherwise, add one to the index
    else y<-y+1
  }
  return(y)
}

### calculate how many patches the leading edge moves each generation
# note this function assumes that only one patch is colonized at start
speed<- function(x,y){ 
  # x is vector of the leading edge patch each generation
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

# CHANGE 

# File names for storing data
#NEED TO INITIALIZE FOR EACH SET OF SIMULATIONS
#ENSURE THAT WD IS IN CORRECT FOLDER

#Final extent of spread
#FILE NAMES SHOULD BE OF THE FORM - ext_tradeoffid_patchlength_sim#.csv
extent<-extfiles_low_0

#Patches colonized each generation
#FILE NAMES SHOULD BE OF THE FORM - spd_tradeoffid_landscapeid_sim#.csv
speed<-spdfiles_low_0

#file name for total speed after all sims
totalspd<-".csv"

# don't change

#Population Spread Matrix
#Adds values to each row for each generation
#Coloumns correspond to patches defined by dvect
spread1<-matrix(0,g_max,patch)
spread1[1,1]<-N_0
spread2<-matrix(0,g_max,patch)
spread2[1,1]<-N_0
spread3<-matrix(0,g_max,patch)
spread3[1,1]<-N_0
spread4<-matrix(0,g_max,patch)
spread4[1,1]<-N_0
spread5<-matrix(0,g_max,patch)
spread5[1,1]<-N_0
spread6<-matrix(0,g_max,patch)
spread6[1,1]<-N_0
spread7<-matrix(0,g_max,patch)
spread7[1,1]<-N_0
spread8<-matrix(0,g_max,patch)
spread8[1,1]<-N_0
spread9<-matrix(0,g_max,patch)
spread9[1,1]<-N_0
spread10<-matrix(0,g_max,patch)
spread10[1,1]<-N_0
spread11<-matrix(0,g_max,patch)
spread11[1,1]<-N_0
spread12<-matrix(0,g_max,patch)
spread12[1,1]<-N_0
spread13<-matrix(0,g_max,patch)
spread13[1,1]<-N_0
spread14<-matrix(0,g_max,patch)
spread14[1,1]<-N_0
spread15<-matrix(0,g_max,patch)
spread15[1,1]<-N_0
spread16<-matrix(0,g_max,patch)
spread16[1,1]<-N_0
spreadttl<-matrix(0,g_max,patch)
spreadttl[1,1]<-N_0*strat_t


#Seeds Dispersed
#matrix for storing seeds produced, resets each generation
seeds<-matrix(NA,strat_t,patch)

#matrix containing number of plants/seeds in each patch after dispersal
#rows are originating patches where seeds were produced,
#coloumns are where they disperse to
seedsd1<-matrix(0,patch,patch)
seedsd2<-matrix(0,patch,patch)
seedsd3<-matrix(0,patch,patch)
seedsd4<-matrix(0,patch,patch)
seedsd5<-matrix(0,patch,patch)
seedsd6<-matrix(0,patch,patch)
seedsd7<-matrix(0,patch,patch)
seedsd8<-matrix(0,patch,patch)
seedsd9<-matrix(0,patch,patch)
seedsd10<-matrix(0,patch,patch)
seedsd11<-matrix(0,patch,patch)
seedsd12<-matrix(0,patch,patch)
seedsd13<-matrix(0,patch,patch)
seedsd14<-matrix(0,patch,patch)
seedsd15<-matrix(0,patch,patch)
seedsd16<-matrix(0,patch,patch)

# Pairwise Distances Matrix for computing dispersal
# Distances between patch i,j for row i 
# and coloumn j
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}

#matrix for storing data from the last generation of spread 
gmax_pop<-matrix(NA,strat_t,patch)
#Vector for the extent of each strategy
ledge<-vector("numeric",strat_t)
# Vector counting winning strategy
wsc<-vector("numeric",strat_t)

# vectors for storing colonized patches
pcol<-matrix(NA, nrow=strat_t+1, ncol=g_max-1)
tcol<-matrix(NA,nrow=nsim,ncol=g_max-1)

####################################################################

#Run Simulation 
for (x in 1:nsim){
  for (i in 1:(g_max-1)){
    total<-spreadttl[i,]
    #produce seeds for each strategy in each patch
    seeds[1,]<-gNty(spread1[i,],total,lambda[1]) 
    seeds[2,]<-gNty(spread2[i,],total,lambda[2])
    seeds[3,]<-gNty(spread3[i,],total,lambda[3])
    seeds[4,]<-gNty(spread4[i,],total,lambda[4])
    seeds[5,]<-gNty(spread5[i,],total,lambda[5])
    seeds[6,]<-gNty(spread6[i,],total,lambda[6])
    seeds[7,]<-gNty(spread7[i,],total,lambda[7])
    seeds[8,]<-gNty(spread8[i,],total,lambda[8])
    seeds[9,]<-gNty(spread9[i,],total,lambda[9]) 
    seeds[10,]<-gNty(spread10[i,],total,lambda[10])
    seeds[11,]<-gNty(spread11[i,],total,lambda[11])
    seeds[12,]<-gNty(spread12[i,],total,lambda[12])
    seeds[13,]<-gNty(spread13[i,],total,lambda[13])
    seeds[14,]<-gNty(spread14[i,],total,lambda[14])
    seeds[15,]<-gNty(spread15[i,],total,lambda[15])
    seeds[16,]<-gNty(spread16[i,],total,lambda[16])
    
    for (j in 1:patch){                    
      #disperse seeds for each strategy in each patch
      seedsd1[j,]<-disperse(seeds[1,j],j,m[1])
      seedsd2[j,]<-disperse(seeds[2,j],j,m[2])
      seedsd3[j,]<-disperse(seeds[3,j],j,m[3])
      seedsd4[j,]<-disperse(seeds[4,j],j,m[4])
      seedsd5[j,]<-disperse(seeds[5,j],j,m[5])
      seedsd6[j,]<-disperse(seeds[6,j],j,m[6])
      seedsd7[j,]<-disperse(seeds[7,j],j,m[7])
      seedsd8[j,]<-disperse(seeds[8,j],j,m[8])
      seedsd9[j,]<-disperse(seeds[9,j],j,m[9])
      seedsd10[j,]<-disperse(seeds[10,j],j,m[10])
      seedsd11[j,]<-disperse(seeds[11,j],j,m[11])
      seedsd12[j,]<-disperse(seeds[12,j],j,m[12])
      seedsd13[j,]<-disperse(seeds[13,j],j,m[13])
      seedsd14[j,]<-disperse(seeds[14,j],j,m[14])
      seedsd15[j,]<-disperse(seeds[15,j],j,m[15])
      seedsd16[j,]<-disperse(seeds[16,j],j,m[16])
    }
    #fill in next row in population matrix
    spread1[i+1,]<-apply(seedsd1,2,sum)    
    spread2[i+1,]<-apply(seedsd2,2,sum)
    spread3[i+1,]<-apply(seedsd3,2,sum)
    spread4[i+1,]<-apply(seedsd4,2,sum)
    spread5[i+1,]<-apply(seedsd5,2,sum)
    spread6[i+1,]<-apply(seedsd6,2,sum)
    spread7[i+1,]<-apply(seedsd7,2,sum)
    spread8[i+1,]<-apply(seedsd8,2,sum)
    spread9[i+1,]<-apply(seedsd9,2,sum)
    spread10[i+1,]<-apply(seedsd10,2,sum)
    spread11[i+1,]<-apply(seedsd11,2,sum)
    spread12[i+1,]<-apply(seedsd12,2,sum)
    spread13[i+1,]<-apply(seedsd13,2,sum)
    spread14[i+1,]<-apply(seedsd14,2,sum)
    spread15[i+1,]<-apply(seedsd15,2,sum)
    spread16[i+1,]<-apply(seedsd16,2,sum)
    #sum over strategies for total population advance
    spreadttl[i+1,]<-spread1[i+1,]+spread2[i+1,]+spread3[i+1,]+spread4[i+1,]+spread5[i+1,]+spread6[i+1,]+spread7[i+1,]+spread8[i+1,]+spread9[i+1,]+spread10[i+1,]+spread11[i+1,]+spread12[i+1,]+spread13[i+1,]+spread14[i+1,]+spread15[i+1,]+spread16[i+1,]
    #calculate how many patches the leading edge has advanced
    pcol[1,i]<-lemove(spread1[i+1,])
    pcol[2,i]<-lemove(spread2[i+1,])
    pcol[3,i]<-lemove(spread3[i+1,])
    pcol[4,i]<-lemove(spread4[i+1,])
    pcol[5,i]<-lemove(spread5[i+1,])
    pcol[6,i]<-lemove(spread6[i+1,])
    pcol[7,i]<-lemove(spread7[i+1,])
    pcol[8,i]<-lemove(spread8[i+1,])
    pcol[9,i]<-lemove(spread9[i+1,])
    pcol[10,i]<-lemove(spread10[i+1,])
    pcol[11,i]<-lemove(spread11[i+1,])
    pcol[12,i]<-lemove(spread12[i+1,])
    pcol[13,i]<-lemove(spread13[i+1,])
    pcol[14,i]<-lemove(spread14[i+1,])
    pcol[15,i]<-lemove(spread15[i+1,])
    pcol[16,i]<-lemove(spread16[i+1,])
    pcol[17,i]<-lemove(spreadttl[i+1,]) 
  }
  # fill in population matrix for last generation 
    gmax_pop[1,]<-spread1[g_max,]
    gmax_pop[2,]<-spread2[g_max,]
    gmax_pop[3,]<-spread3[g_max,]
    gmax_pop[4,]<-spread4[g_max,]
    gmax_pop[5,]<-spread5[g_max,]
    gmax_pop[6,]<-spread6[g_max,]
    gmax_pop[7,]<-spread7[g_max,]
    gmax_pop[8,]<-spread8[g_max,]
    gmax_pop[9,]<-spread9[g_max,]
    gmax_pop[10,]<-spread10[g_max,]
    gmax_pop[11,]<-spread11[g_max,]
    gmax_pop[12,]<-spread12[g_max,]
    gmax_pop[13,]<-spread13[g_max,]
    gmax_pop[14,]<-spread14[g_max,]
    gmax_pop[15,]<-spread15[g_max,]
    gmax_pop[16,]<-spread16[g_max,]
  
    for (i in 1:strat_t){
      # calculate the leading edge patch for each strategy
      # at the last generation
      ledge[i]<-lemove(gmax_pop[i,])
    }
    for (i in 1:strat_t){
      # find the strategies present at the leading edge
      if (max(ledge)==ledge[i])
        wsc[i]<-wsc[i]+1
    }
    tcol[x,]<-pcol[17,]
    
    # save extent and speed matrices to CSV
    write.table(gmax_pop,file=extent[x])
    write.table(pcol,file=speed[x])
 }
#save total speed for each generation to CSV
 write.table(tcol,file=totalspd)
