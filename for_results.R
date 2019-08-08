## FUNCTIONS FOR EXTRACTING DATA

# Count and identify sims where the whole population dies
extinct<-function(x){ #x is vector of results matrix -- population in each patch at sim. end,
  n.ext<-c()          # requires strat_t and patch to match the simulation inputs
  for (i in 1:length(x)){
    if (checkext(x[[i]]) == TRUE){
      n.ext<-c(n.ext,i)
    }
  }
  return(n.ext)
}

# extinct helper
checkext<-function(y){
  for (i in 1:strat_t){
    for (j in 1:patch){
      if (y[i,j] != 0)
        return(FALSE)
    }
  }
  return(TRUE)
}

# count and identify strategies that spread past first patch
gapcross<-function(x,p){    # x is total colonized matrix
  n.cross<-c()              # p is the length of habitable patches 
  for (i in 1:nsim){     # function requires g_max,nsim parameters to match simulations    
    if (x[i,(g_max-1)] >= p){
      n.cross<-c(n.cross,i)
    }
  }
  return(n.cross)
}

# construct vector of winning strategies excluding those that don't cross a gap
winners<-function(x,g){           # x is list of extent matrices
  ws<-vector("numeric", strat_t)  # g is the output of "gapcross" for sim. set
  for (i in 1:length(x)){         # fn requires strat_t matching, fn "lemove"
    for (j in 1:length(g)){       # simulation list MUST be in order of sim #
      if (i == g[j]){
       ws<-addwinner(x[[i]],ws) 
      }
    }
  }
  return(ws)
}

# winners helper
addwinner<-function(y,z){
  ledge<-vector("numeric",strat_t)
  for (i in 1:strat_t){
    ledge[i]<-lemove(y[i,])
  }
  for (j in 1:strat_t){
    if (max(ledge)==ledge[j])
      z[j]<-z[j]+1
  }
  return(z)
}

###########################################################################################################

# PLOTS

#Plot the distribution of strategies at the last generation
plot(spread1[g_max,], type="l", col="hotpink",ylim=c(0,300),main="Population after 20 Generations of Spread", 
     xlab="Patch Distance from Origin", ylab="Number of Individuals",lwd=2)
lines(spread2[g_max,], col="violetred",lwd=2)
lines(spread3[g_max,], col="firebrick",lwd=2)
lines(spread4[g_max,], col="red",lwd=2)
lines(spread5[g_max,], col="darkorange",lwd=2)
lines(spread6[g_max,], col="tan1",lwd=2)
lines(spread7[g_max,], col="goldenrod1",lwd=2)
lines(spread8[g_max,], col="yellow3",lwd=2)
lines(spread9[g_max,], col="yellowgreen",lwd=2)
lines(spread10[g_max,], col="springgreen1",lwd=2)
lines(spread11[g_max,], col="forestgreen",lwd=2)
lines(spread12[g_max,], col="darkturquoise",lwd=2)
lines(spread13[g_max,], col="deepskyblue1",lwd=2)
lines(spread14[g_max,], col="royalblue2",lwd=2)
lines(spread15[g_max,], col="mediumblue",lwd=2)
lines(spread16[g_max,], col="navy",lwd=2)
legend("topright", legend=c("Dispersive", "","","","","","","","","","","","","","", "Fecund"),
       col=c("hotpink","violetred","firebrick","red","darkorange","tan1","goldenrod1","yellow3","yellowgreen",
             "springgreen1","forestgreen","darkturquoise","deepskyblue1","royalblue2","mediumblue","navy"),
       lty=1,seg.len = 1,y.intersp = 0.5)

#Plot barplotbarplot of winning strategy each simulation
winners<-as.matrix(winningstrat_d02)
barplot(winners[1,], space=0, 
        sub="d=0.25, no gaps", 
        names.arg = 1:strat_t, 
        ylab="Number of replications at the leading edge", 
        xlab="Strategy number",
        ylim=c(0,50))
mtext("<- Dispersive                                                                                           Fecund ->", 
      side=1,
      line=2)

winners2<-as.matrix(winningstrat_d15)
barplot(winners2[1,], space=0, 
        sub="d=1.5, no gaps", 
        names.arg = 1:strat_t, 
        ylab="Number of replications at the leading edge", 
        xlab="Strategy number",
        ylim=c(0,50))
mtext("<- Dispersive                                                                                           Fecund ->", 
      side=1,
      line=2)

# for patches colonized plot
mean_speed<-matrix(NA,3,39)
speed_sd_up<-matrix(NA,3,39)
speed_sd_down<-matrix(NA,3,39)

mean_speed[1,]<-apply(totalspd_d02_0,2,mean)
mean_speed[3,]<-apply(totalspd_d02_18,2,mean)
mean_speed[2,]<-apply(totalspd_d02_88,2,mean)

speed_sd<-apply(totalspd_d02_0,2,sd)
speed_sd_up[1,]<-mean_speed[1,]+speed_sd
speed_sd_down[1,]<-mean_speed[1,]-speed_sd

speed_sd<-apply(totalspd_d02_18,2,sd)
speed_sd_up[3,]<-mean_speed[3,]+speed_sd
speed_sd_down[3,]<-mean_speed[3,]-speed_sd

speed_sd<-apply(totalspd_d02_88,2,sd)
speed_sd_up[2,]<-mean_speed[2,]+speed_sd
speed_sd_down[2,]<-mean_speed[2,]-speed_sd

mean_speed2<-matrix(NA,3,39)
speed_sd_up2<-matrix(NA,3,39)
speed_sd_down2<-matrix(NA,3,39)

mean_speed2[1,]<-apply(totalspd_d15_0,2,mean)
mean_speed2[3,]<-apply(totalspd_d15_18,2,mean)
mean_speed2[2,]<-apply(totalspd_d15_88,2,mean)

speed_sd<-apply(totalspd_d15_0,2,sd)
speed_sd_up2[1,]<-mean_speed2[1,]+speed_sd
speed_sd_down2[1,]<-mean_speed2[1,]-speed_sd

speed_sd<-apply(totalspd_d15_18,2,sd)
speed_sd_up2[3,]<-mean_speed2[3,]+speed_sd
speed_sd_down2[3,]<-mean_speed2[3,]-speed_sd

speed_sd<-apply(totalspd_d15_88,2,sd)
speed_sd_up2[2,]<-mean_speed2[2,]+speed_sd
speed_sd_down2[2,]<-mean_speed2[2,]-speed_sd

par(mfrow=c(1,2))
plot(NULL,
     xlab="Generation",
     ylab="Distance of furthest patch colonized",
     sub="d=0.25",
     xlim=c(5,33),
     ylim=c(0,200))
gen<-2:35
x<-0
y<-vector("numeric",39)
for (i in 1:100){
  y<-totalspd_d02_0[i,]+x
  lines(x=1:39,
        y=y,
        lty=2,
        col="#fbb4ae") 
  x<-x+0.01
}

# polygon(x=c(gen,rev(gen)),
#         y=c(speed_sd_down[1,2:35],rev(speed_sd_up[1,2:35])),
#         col="#e41a1c",
#         border=NA)
lines(mean_speed[1,],
      lwd=3,
      col="black")
lines(speed_sd_up[1,],
      lwd=2,
      col="#e41a1c"
      )
lines(speed_sd_down[1,],
      lwd=2,
      col="#e41a1c"
)
x<-0
y<-vector("numeric",39)
for (i in 1:100){
  y<-totalspd_d02_88[i,]+x
  lines(x=1:39,
        y=y,
        lty=2,
        col="#b3cde3") 
  x<-x+0.01
}
# polygon(x=c(gen,rev(gen)),
#         y=c(speed_sd_down[2,2:35],rev(speed_sd_up[2,2:35])),
#         col="#b3cde3",
#         border=NA)
lines(mean_speed[2,],
      lwd=3,
      col="black")
lines(speed_sd_up[2,],
      lwd=2,
      col="#377eb8"
)
lines(speed_sd_down[2,],
      lwd=2,
      col="#377eb8"
)


plot(NULL,
     xlab="Generation",
     ylab="",
     sub="d=1.5",
     xlim=c(5,33),
     ylim=c(0,200))
# polygon(x=c(gen,rev(gen)),
#         y=c(speed_sd_down2[1,2:35],rev(speed_sd_up2[1,2:35])),
#         col="#fbb4ae",
#         border=NA)
x<-0
y<-vector("numeric",39)
for (i in 1:100){
  y<-totalspd_d15_0[i,]+x
  lines(x=1:39,
        y=y,
        lty=2,
        col="#fbb4ae") 
  x<-x+0.01
}
lines(mean_speed2[1,],
      lwd=3,
      col="black")
lines(speed_sd_up2[1,],
      lwd=2,
      col="#e41a1c"
)
lines(speed_sd_down2[1,],
      lwd=2,
      col="#e41a1c"
)
# polygon(x=c(gen,rev(gen)),
#         y=c(speed_sd_down2[2,2:35],rev(speed_sd_up2[2,2:35])),
#         col="#b3cde3",
#         border=NA)
x<-0
y<-vector("numeric",39)
for (i in 1:100){
  y<-totalspd_d15_88[i,]+x
  lines(x=1:39,
        y=y,
        lty=2,
        col="#b3cde3") 
  x<-x+0.01
}
lines(mean_speed2[2,],
      lwd=3,
      col="black")
lines(speed_sd_up2[2,],
      lwd=2,
      col="#377eb8"
)
lines(speed_sd_down2[2,],
      lwd=2,
      col="#377eb8"
)
legend(x="topleft",
       legend=c("Mean across simulations",
                "1 SD - continuous habitat",
                "1 SD - length 8 patches / gaps",
                "Simulations - continuous habitat",
                "Simulations - length 8 patches / gaps"),
       col=c("black","#e41a1c","#377eb8","#fbb4ae","#b3cde3"),
       lwd=c(3,2,2,1,1),
       lty=c(1,1,1,2,2))

# lines(mean_speed[3,],
#       lwd=3,
#       col="black")
# lines(speed_sd_up[3,],
#       lwd=2,
#       col="#4daf4a",
# )
# lines(speed_sd_down[3,],
#       lwd=2,
#       col="#4daf4a",
# )

# Histograms of winning strat
par(mfrow=c(2,3))
winners_all02<-as.matrix(winningstrat_d02)
winners_all15<-as.matrix(winning_strat_15)

winners_sub02<-matrix(NA,3,16)
winners_sub15<-matrix(NA,3,16)
winners_sub02[1,]<-winners_all02[1,]
winners_sub15[1,]<-winners_all02[1,]

winners_sub02[2,]<-c(2,17,14,6,1,2,0,1,1,0,0,0,0,0,0,0)
winners_sub02[3,]<-c(1,2,2,1,0,1,0,0,1,0,0,0,0,1,0,0)

ncross02<-matrix(NA,3,16)
ncross02<-winners_all02-winners_sub02

wd02_88<-matrix(NA,2,16)
wd02_88[1,]<-ncross02[2,]
wd02_88[2,]<-winners_sub02[2,]
barplot(winners_all02[1,],
        col="#e41a1c",
        ylab="# of replicates",
        names.arg=1:16,
        ylim=c(0,100))
barplot(wd02_88,
        col=c('#b3cde3',"#377eb8"),
        names.arg=1:16,
        ylim=c(0,100))
legend("topleft",
       legend=c("Spread beyond 1st gap",
                "Did not spread beyond 1st gap"),
       col=c("#377eb8",'#b3cde3'),
       pch=15)

wd02_18<-matrix(NA,2,16)
wd02_18[1,]<-ncross02[3,]
wd02_18[2,]<-winners_sub02[3,]
barplot(wd02_18,
        col=c("#ccebc5","#4daf4a"),
        names.arg=1:16,
        ylim=c(0,100))
legend("bottomleft",
       legend=c("Spread beyond 1st gap",
                "Did not spread beyond 1st gap"),
       col=c("#4daf4a",'#ccebc5'),
       pch=15)

winners_sub15[2,]<-c(3,3,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
winners_sub15[3,]<-c(0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0)

ncross15<-matrix(NA,3,16)
ncross15<-winners_all15-winners_sub15

wd15_88<-matrix(NA,2,16)
wd15_88[1,]<-ncross15[2,]
wd15_88[2,]<-winners_sub15[2,]
barplot(winners_all15[1,],
        col="#e41a1c",
        ylab="# of replicates",
        xlab = "Strategy number",
        names.arg=1:16,
        ylim=c(0,100))
barplot(wd15_88,
        col=c('#b3cde3',"#377eb8"),
        names.arg=1:16,
        xlab="Strategy number",
        ylim=c(0,100))
legend("topleft",
       legend=c("Spread beyond 1st gap",
                "Did not spread beyond 1st gap"),
       col=c("#377eb8",'#b3cde3'),
       pch=15)

wd15_18<-matrix(NA,2,16)
wd15_18[1,]<-ncross15[3,]
wd15_18[2,]<-winners_sub15[3,]
barplot(wd15_18,
        col=c("#ccebc5","#4daf4a"),
        names.arg=1:16,
        xlab="Strategy number",
        ylim=c(0,100))
legend("bottomleft",
       legend=c("Spread beyond 1st gap",
                "Did not spread beyond 1st gap"),
       col=c("#4daf4a",'#ccebc5'),
       pch=15)
mtext("No gaps", side=1,line=-30, outer = TRUE, adj=0.15,)
mtext("Patches & gaps are length 8", side=1,line=-30, outer = TRUE, adj=0.5,)
mtext("Single patches with length 8 gaps", side=1,line=-30, outer = TRUE, adj=0.9)
mtext("d=0.25",side=4,line=-2,outer=TRUE,adj=0.75)
mtext("d=1.5",side=4,line=-2,outer=TRUE,adj=0.25)

# patches per gen boxplots
patchpg02<-matrix(NA,nsim,3)
patchpg15<-matrix(NA,nsim,3)

ppg_02_0<-matrix(NA,nsim,g_max-1)
ppg_02_0[,1]<-totalspd_d02_0[,1]
ppg_02_88<-matrix(NA,nsim,g_max-1)
ppg_02_88[,1]<-totalspd_d02_88[,1]
ppg_02_18<-matrix(NA,nsim,g_max-1)
ppg_02_18[,1]<-totalspd_d02_18[,1]
ppg_15_0<-matrix(NA,nsim,g_max-1)
ppg_15_0[,1]<-totalspd_d15_0[,1]
ppg_15_88<-matrix(NA,nsim,g_max-1)
ppg_15_88[,1]<-totalspd_d15_88[,1]
ppg_15_18<-matrix(NA,nsim,g_max-1)
ppg_15_18[,1]<-totalspd_d15_18[,1]

for (i in 1:nsim){
  for (j in 2:39){
    ppg_02_0[i,j]<-totalspd_d02_0[i,j]-totalspd_d02_0[i,j-1]
  }
}

for (i in 1:nsim){
  for (j in 2:39){
    ppg_02_88[i,j]<-totalspd_d02_88[i,j]-totalspd_d02_88[i,j-1]
  }
}

for (i in 1:nsim){
  for (j in 2:39){
    ppg_02_18[i,j]<-totalspd_d02_18[i,j]-totalspd_d02_18[i,j-1]
  }
}

for (i in 1:nsim){
  for (j in 2:39){
    ppg_15_0[i,j]<-totalspd_d15_0[i,j]-totalspd_d15_0[i,j-1]
  }
}

for (i in 1:nsim){
  for (j in 2:39){
    ppg_15_88[i,j]<-totalspd_d15_88[i,j]-totalspd_d15_88[i,j-1]
  }
}

for (i in 1:nsim){
  for (j in 2:39){
    ppg_15_18[i,j]<-totalspd_d15_18[i,j]-totalspd_d15_18[i,j-1]
  }
}

patchpg02[,1]<-apply(ppg_02_0,1,mean)
patchpg02[,3]<-apply(ppg_02_18,1,mean)
patchpg02[,2]<-apply(ppg_02_88,1,mean)
patchpg15[,1]<-apply(ppg_15_0,1,mean)
patchpg15[,3]<-apply(ppg_15_18,1,mean)
patchpg15[,2]<-apply(ppg_15_88,1,mean)

par(mfrow=c(1,2))
boxplot(patchpg02,
        ylab="mean patches advanced per generation",
        sub="d=0.25",
        border=c("#e41a1c","#377eb8","#4daf4a"),
        col=c("#fbb4ae","#b3cde3","#ccebc5"),
        names=c("no gaps","8-8","1-8")
        )
boxplot(patchpg15,
        sub="d=1.5",
        border=c("#e41a1c","#377eb8","#4daf4a"),
        col=c("#fbb4ae","#b3cde3","#ccebc5"),
        names=c("no gaps","8-8","1-8")
        )