#########################################################

# MASTER SCRIPT

# parameters

#intrinsic rate of increase (lowest)
lambda1 <- 35
#intrinsic rate of increase
lambda2 <- 35
#intrinsic rate of increase
lambda3 <- 35
#intrinsic rate of increase
lambda4 <- 35
#intrinsic rate of increase
lambda5 <- 35
#intrinsic rate of increase
lambda6 <- 35
#intrinsic rate of increase
lambda7 <- 35
#intrinsic rate of increase (highest)
lambda8 <- 35
#intraspecific competition
alpha <- .2
#distance decay rate (highest)
m1 <- 0.5
#distance decay rate 
m2<-0.75
#distance decay rate 
m3<-1.25
#distance decay rate 
m4<-1.75
#distance decay rate 
m5<-2.25
#distance decay rate 
m6<-2.75
#distance decay rate 
m7<-3.25
#distance decay rate 
m8<-3.75
#starting population for each strategy
N_0 <- 2
# number of simulations to run
nsim<-1
#number of generations to simulate 
g_max <- 10
#distance between patches vector
dpatch <- 1
#Define distance vector (for now must be integer distances between patches)
dvect <- seq(from=0,length.out = 50,by=dpatch)
#number of patches
patch <- length(dvect)
# p1<-seq(from=0,length.out=3,by=1)
# p2<-seq(from=8,length.out=3,by=1)
# p3<-seq(from=13,length.out=5,by=1)
# dvect<-c(p1,p2,p3)

##########################################################

#functions

#seed production function
gNty <- function(j,k,lambda){ #j is subpopulation, k is total
  seeds<-vector()
  jnext<-(lambda*j)/(1+alpha*(k-1))
  seeds<-c(seeds,jnext)
  return(round(seeds,0))
}

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
  moved<-dvect[moved]
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
spreadttl<-matrix(0,g_max,patch)
spreadttl[1,1]<-N_0*8


#Seeds Dispersed
#vector for storing seeds produced, resets each generation
seeds1<-vector("numeric", patch)
seeds2<-vector("numeric",patch)
seeds3<-vector("numeric",patch)
seeds4<-vector("numeric",patch)
seeds5<-vector("numeric",patch)
seeds6<-vector("numeric",patch)
seeds7<-vector("numeric",patch)
seeds8<-vector("numeric",patch)
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

#Pairwise Distances Matrix
#Distances between patch i,j for row i 
#and coloumn j
pwdist<-matrix(NA,patch,patch)
for (i in 1:patch){
  for (j in 1:patch){
    pwdist[i,j]<-dvect[j]-dvect[i]
  }
}
#matrix for the last generation of spread 
gmax_pop<-matrix(NA,8,patch)
#Vector for the extent of each strategy
ledge<-vector("numeric",8)
# Vector counting winning strategy
wsc<-vector("numeric",8)

# vectors for storing colonized patches
#   col1<-vector("numeric", g_max-1)
#  col2<-vector("numeric", g_max-1)
#  col3<-vector("numeric", g_max-1)
#  col4<-vector("numeric", g_max-1)
#  col5<-vector("numeric", g_max-1)
#  col6<-vector("numeric", g_max-1)
#  col7<-vector("numeric", g_max-1)
#  col8<-vector("numeric", g_max-1)
colt<-vector("numeric", g_max-1)
# 
# #Speed vectors 
# speedv1<-vector("numeric", g_max)
# speedv2<-vector("numeric",g_max)
# speedv3<-vector("numeric",g_max)
# speedv4<-vector("numeric",g_max)
# speedv5<-vector("numeric",g_max)
# speedv6<-vector("numeric",g_max)
# speedv7<-vector("numeric",g_max)
# speedv8<-vector("numeric",g_max)
# speedt<-vector("numeric", g_max)

#Matrix for storing advance per gen.
pmg1<-matrix(NA,nsim,g_max-1)

# Average speed vectors
# speedmn1<-vector("numeric",nsim)
# speedmn2<-vector("numeric",nsim)
# speedmn3<-vector("numeric",nsim)
# speedmn4<-vector("numeric",nsim)
# speedmn5<-vector("numeric",nsim)
# speedmn6<-vector("numeric",nsim)
# speedmn7<-vector("numeric",nsim)
# speedmn8<-vector("numeric",nsim)
# speedmnt<-vector("numeric",nsim)

# File names for storing data
#NEED TO INITIALIZE FOR EACH SET OF SIMULATIONS

#Final extent of spread 
#FILE NAMES SHOULD BE OF THE FORM - tradeoffid_sim#.csv
extent<-c("_01.csv","_02.csv")


####################################################################

#Run Simulation 
for (x in 1:nsim){                       #for each simulation
  for (i in 1:(g_max-1)){                 #for each generation
    total<-spreadttl[i,]                   #compute total population
    seeds1<-gNty(spread1[i,],total,lambda1) #make seeds
    seeds2<-gNty(spread2[i,],total,lambda2)
    seeds3<-gNty(spread3[i,],total,lambda3)
    seeds4<-gNty(spread4[i,],total,lambda4)
    seeds5<-gNty(spread5[i,],total,lambda5)
    seeds6<-gNty(spread6[i,],total,lambda6)
    seeds7<-gNty(spread7[i,],total,lambda7)
    seeds8<-gNty(spread8[i,],total,lambda8)
    
    for (j in 1:patch){                     #disperse seeds
      seedsd1[j,]<-disperse(seeds1[j],j,m1)
      seedsd2[j,]<-disperse(seeds2[j],j,m2)
      seedsd3[j,]<-disperse(seeds3[j],j,m3)
      seedsd4[j,]<-disperse(seeds4[j],j,m4)
      seedsd5[j,]<-disperse(seeds5[j],j,m5)
      seedsd6[j,]<-disperse(seeds6[j],j,m6)
      seedsd7[j,]<-disperse(seeds7[j],j,m7)
      seedsd8[j,]<-disperse(seeds8[j],j,m8)
    }
    spread1[i+1,]<-apply(seedsd1,2,sum)     #fill in next row in population matrix
    spread2[i+1,]<-apply(seedsd2,2,sum)
    spread3[i+1,]<-apply(seedsd3,2,sum)
    spread4[i+1,]<-apply(seedsd4,2,sum)
    spread5[i+1,]<-apply(seedsd5,2,sum)
    spread6[i+1,]<-apply(seedsd6,2,sum)
    spread7[i+1,]<-apply(seedsd7,2,sum)
    spread8[i+1,]<-apply(seedsd8,2,sum)
    spreadttl[i+1,]<-spread1[i+1,]+spread2[i+1,]+spread3[i+1,]+spread4[i+1,]+spread5[i+1,]+spread6[i+1,]+spread7[i+1,]+spread8[i+1,]
    
    
    # col1[i]<-lemove(spread1[i+1,])#calculate how many patches the leading edge has advanced
    # col2[i]<-lemove(spread2[i+1,])
    # col3[i]<-lemove(spread2[i+1,])
    # col4[i]<-lemove(spread2[i+1,])
    # col5[i]<-lemove(spread2[i+1,])
    # col6[i]<-lemove(spread2[i+1,])
    # col7[i]<-lemove(spread2[i+1,])
    # col8[i]<-lemove(spread2[i+1,])
    colt[i]<-lemove(spreadttl[i+1,]) 
    
    gmax_pop[1,]<-spread1[g_max,]
    gmax_pop[2,]<-spread2[g_max,]
    gmax_pop[3,]<-spread3[g_max,]
    gmax_pop[4,]<-spread4[g_max,]
    gmax_pop[5,]<-spread5[g_max,]
    gmax_pop[6,]<-spread6[g_max,]
    gmax_pop[7,]<-spread7[g_max,]
    gmax_pop[8,]<-spread8[g_max,]
  }
  for (i in 1:8){
    ledge[i]<-lemove(gmax_pop[i,])
  }
  for (i in 1:8){
    if (max(ledge)==ledge[i])
      wsc[i]<-wsc[i]+1
  }
  
  
  pmg1[x,]<-colt                       #fill in patches moved matrix
  write.table(gmax_pop,file=extent[1])
  
  # speedv1<-speed(col1,speedv1) #combine advances to get invasion velocity of leading edge
  # speedv2<-speed(col2,speedv2)
  # speedv3<-speed(col2,speedv3)
  # speedv4<-speed(col2,speedv4)
  # speedv5<-speed(col2,speedv5)
  # speedv6<-speed(col2,speedv6)
  # speedv7<-speed(col2,speedv7)
  # speedv8<-speed(col2,speedv8)
  # speedt<-speed(colt,speedt)
  # speedm1<-mean(speedv1) # average invasion velocity over time
  # speedm2<-mean(speedv2)
  # speedm3<-mean(speedv3)
  # speedm4<-mean(speedv4)
  # speedm5<-mean(speedv5)
  # speedm6<-mean(speedv6)
  # speedm7<-mean(speedv7)
  # speedm8<-mean(speedv8)
  # speedmt<-mean(speedt)
  
  #Plot the distribution of strategies at the last generation
  plot(spread1[g_max,], type="l", col="red",ylim=c(0,300),main="Population after 20 Generations of Spread",  xlab="Patch Distance from Origin", ylab="Number of Individuals",lwd=2)
  lines(spread2[g_max,], col="darkorange",lwd=2)
  lines(spread3[g_max,], col="goldenrod",lwd=2)
  lines(spread4[g_max,], col="yellowgreen",lwd=2)
  lines(spread5[g_max,], col="springgreen2",lwd=2)
  lines(spread6[g_max,], col="turquoise3",lwd=2)
  lines(spread7[g_max,], col="royalblue2",lwd=2)
  lines(spread8[g_max,], col="navy",lwd=2)
  legend("topright", legend=c("Dispersive", "","","","","","", " Fecund"),col=c("red","darkorange","goldenrod","yellowgreen","springgreen2","turquoise3","royalblue2","  navy"), lty=1,seg.len = 1,y.intersp = 0.5)
  
  # speedv1<-speed(col1,speedv1) #combine advances to get invasion velocity of leading edge
  # speedv2<-speed(col2,speedv2)
  # speedv3<-speed(col3,speedv3)
  # speedv4<-speed(col4,speedv4)
  # speedv5<-speed(col5,speedv5)
  # speedv6<-speed(col6,speedv6)
  # speedv7<-speed(col7,speedv7)
  # speedv8<-speed(col8,speedv8)
  # speedt<-speed(colt,speedt)
  
  #   plot(speedv1, type="l", xlab="Generation Number", ylab="Invasion Velocity (multiples of MDD)", lwd=2, lty=2,col="darkorange", ylim=c(0,10))
  #   lines(speedv2, col="red", lwd=2,lty=2)
  #   lines(speedv3, col="brown", lwd=2,lty=2)
  #   lines(speedv4, col="green", lwd=2,lty=2)
  #   lines(speedv5, col="darkgreen", lwd=2,lty=2)
  #   lines(speedv6, col="darkblue", lwd=2,lty=2)
  #   lines(speedv7, col="blue", lwd=2,lty=2)
  #   lines(speedv8, col="blueviolet", lwd=2,lty=2)
  #   lines(speedt,lwd=2)
  #   legend("topright", legend=c("Total Population", "Highest mean dispersal distance", "Highest Fecundity"), col=c("black", "red","blueviolet"), lty=1, seg.len=1.5)
  # speedmn1[x]<-(col1[g_max-1]-1)/g_max
  # speedmn2[x]<-(col2[g_max-1]-1)/g_max
  # speedmn3[x]<-(col3[g_max-1]-1)/g_max
  # speedmn4[x]<-(col4[g_max-1]-1)/g_max
  # speedmn5[x]<-(col5[g_max-1]-1)/g_max
  # speedmn6[x]<-(col6[g_max-1]-1)/g_max
  # speedmn7[x]<-(col7[g_max-1]-1)/g_max
  # speedmn8[x]<-(col8[g_max-1]-1)/g_max
  # speedmnt[x]<-(colt[g_max-1]-1)/g_max
}
# plot(speedmn8, type="p", xlab="Index", ylab="Average Invasion Velocity", ylim=c(-3,5),pch=16,col="blueviolet")
# points(speedmn1,col="red",pch=16)
# points(speedmn2,col="darkorange",pch=16)
# points(speedmn3,col="brown",pch=16)
# points(speedmn4,col="green",pch=16)
# points(speedmn5,col="darkgreen",pch=16)
# points(speedmn6,col="darkblue",pch=16)
# points(speedmn7,col="blue",pch=16)
# points(speedmnt,pch=16)


mppg1<-vector("numeric",g_max)          #create vector for mean speed
mppg1<-apply(pmg1,2,mean)                #compute mean advance each gen.

#Plot barplotbarplot of winning strategy each simulation
barplot(wsc, space=0, names.arg=c("Dispersive","","","","","","","Fecund"), 
        main="Strategy at the Leading Edge", sub="Count Over all Simulations", ylab="Number of Invasions", ylim=c(0,25))

#Plot the advance across replicates + mean
plot(pmg1[1,],main="Movement of Leading Edge over 20 Generations",ylab="Distance from Starting Patch", xlab="Generation",
     type="l", lty=2,col="grey",ylim=c(0,patch))
lines(pmg1[2,],lty=2,col="grey")
lines(pmg1[3,],lty=2,col="grey")
lines(pmg1[4,],lty=2,col="grey")
lines(pmg1[5,],lty=2,col="grey")
lines(pmg1[6,],lty=2,col="grey")
lines(pmg1[7,],lty=2,col="grey")
lines(pmg1[8,],lty=2,col="grey")
lines(pmg1[9,],lty=2,col="grey")
lines(pmg1[10,],lty=2,col="grey")
lines(mppg1,lty=1,lwd=2)
legend(x="bottomright", legend=c("Mean","Replicates"),lty=c(1,2),col=c("black","grey"))
```