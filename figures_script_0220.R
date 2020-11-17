#### PLOTS ##########################################################################
#####################################################################################

#### define colour scheme:

# Low cost colours
lowcol_main<-"#2171b5"
lowcol_light<- "#c6dbef"

#Medium cost colours
medcol_main<- "#969696"
medcol_light<- "#d9d9d9"

#High cost colours
highcol_main<- "#cb181d"
highcol_light<- "#fcbba1"

#Strategy colours orig colors
m1col_main<-"#07306c"
m1col_light<-"#a0a6d2"

m2col_main<-"#6b2470"
m2col_light<-"#c9a0ca"

m3col_main<-"#a80051"
m3col_light<-"#e89eb0"

m4col_main<-"#be3216"
m4col_light<-"#eea691"

#gap length colours
gapscol_main<-"#005a32"
gapscol_light<-'#a1d99b'

g1col_main<-"#03582e"
g1col_light<-"#8eba9b"

g2col_main<-"#3d5000"
g2col_light<-"#acb486"

g3col_main<-"#633f00"
g3col_light<-"#c9a984"

g4col_main<-"#831e0a"
g4col_light<-"#de9f8d"

# #rb colours
# #Strategy colours
# m1col_main<-"#2171b5"
# m1col_light<-"#c6dbef"
# 
# m2col_main<-"#969696"
# m2col_light<-"#d9d9d9"
# 
# m3col_main<-"#969696"
# m3col_light<-"#d9d9d9"
# 
# m4col_main<-"#cb181d"
# m4col_light<-"#fcbba1"
# 
# #gap length colours
# gapscol_main<-"#2171b5"
# gapscol_light<-"#c6dbef"
# 
# g1col_main<-"#2171b5"
# g1col_light<-"#c6dbef"
# 
# g2col_main<-"#969696"
# g2col_light<-"#d9d9d9"
# 
# g3col_main<-"#969696"
# g3col_light<-"#d9d9d9"
# 
# g4col_main<-"#cb181d"
# g4col_light<-"#fcbba1"
#####################################################################################

#### FIGURE 1: Trade-off curves and strategies ######################################

#Trade-off function (from parameter setup script)
set_curve <- function(c,m,d){    # c is a co-efficient to set maximum lambda
                                 # m is the input dispersal parameter
                                 # d is the exponent, defines the shape of the curve
                                 # k sets the minimum lambda
  lambda<-c*((m)/2)^d
  return(lambda)
}

#Set co-efficient c and dispersal strategies m 
c<-70
m<-c(0.702,0.78,0.86,0.94,1.02,1.1,1.18,1.26,1.34,1.42,1.5,1.58,1.66,1.74,1.82,1.9)

#Smaller intervals of m to smooth curve
m_line<-c(0.68,0.703,0.705,0.707,0.709,0.71,0.72,0.73,0.75,0.77,0.8,0.83,0.85,0.9,0.95,
          1,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.7,1.95,2)

# set lambdas for 16 strategies

lambda.low<-set_curve(c,m,0.25)
lambda.mid<-set_curve(c,m,0.6)
lambda.high<-set_curve(c,m,1.25)

# set lambdas for smooth curve

lambda.low_line<-set_curve(c,m_line,0.25)
lambda.mid_line<-set_curve(c,m_line,0.6)
lambda.high_line<-set_curve(c,m_line,1.25)

# make plot

plot(lambda.high_line~m_line, ylim=c(15,70),xlim=c(0.68,2),
     xlab=expression(italic(m)),
     ylab=expression(lambda),type="l",lwd=3,col=highcol_main, cex.lab=1.5)
lines(x=m_line,y=lambda.mid_line,lwd=3,col=medcol_main)
lines(x=m_line,y=lambda.low_line,lwd=3,col=lowcol_main)
points(x=m,y=lambda.high,pch=16,col=highcol_main)
points(x=m,y=lambda.mid,pch=16,col=medcol_main)
points(x=m,y=lambda.low,pch=16,col=lowcol_main)
#text(0.9,23,"H",cex=2)
#text(0.9,47,"M",cex=2)
#text(0.9,60,"L",cex=2)
 legend(x="bottomright",
       col=c(lowcol_main,medcol_main,highcol_main,"black"), 
       legend=c("d=0.25 (Low)","d=0.6 (Med.)","d=1.25 (High)","Strategies Used"),
      lty=c(1,1,1,NA), lwd=c(3,3,3,NA),
      pch=c(NA,NA,NA,16), bty="n",cex=1.3)
 
#################################################################################################
 
#### FIGURE 2: Evolutionary Outcomes ############################################################
 
 # Plot continuous, 4-4,8-8 and 1-8 in two rows
 par(mfrow=c(2,4))

 # Transform to matrices
 winningstrat0<-as.matrix(winningstrat0)
 winningstrat44<-as.matrix(winningstrat44)
 winningstrat88_stuck<-as.matrix(winningstrat88_stuck)
 winningstrat88_crossing<-as.matrix(winningstrat88_crossing)
 winningstrat18_stuck<-as.matrix(winningstrat18_stuck)
 winningstrat18_crossing<-as.matrix(winningstrat18_crossing)
 
 #Plot high cost, continuous
 par(mar=c(2.61,3.21,4.1,0))
 barplot(winningstrat0[1,],
         col=highcol_main,
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 mtext("A",side=3,line=1,cex=2)
 
 #Plot high cost, 4-4
 par(mar=c(2.61,3.21,4.1,0))
 barplot(winningstrat44[1,],
         col=highcol_main,
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 mtext("B",side=3,line=1,cex=2)
 
 #Plot high cost, 8-8
 par(mar=c(2.61,3.21,4.1,0))
 winners_h88<-matrix(NA,2,16)
 winners_h88[1,]<-winningstrat88_stuck[1,]
 winners_h88[2,]<-winningstrat88_crossing[1,]
 barplot(winners_h88,
         col=c(highcol_light,highcol_main),
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 mtext("C",side=3,line=1,cex=2)
 
 #Plot high cost, 1-8
 par(mar=c(2.61,3.1,4.1,1.1))
 winners_h18<-matrix(NA,2,16)
 winners_h18[1,]<-winningstrat18_stuck[1,]
 winners_h18[2,]<-winningstrat18_crossing[1,]
 barplot(winners_h18,
         col=c(highcol_light,highcol_main),
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 legend("topleft",
        legend=c("Advanced","Did not advance"),
        col=c(highcol_main,highcol_light),
        pch=15,
        cex=1.5,
        pt.cex=2,
        bty="n"
 )
 mtext("D",side=3,line=1,cex=2)
 
#Plot low cost continuous
 par(mar=c(4.1,3.21,2.61,0))
 barplot(winningstrat0[3,],
         col=lowcol_main,
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)

 
 #plot low cost 4-4
 par(mar=c(4.1,3.21,2.61,0))
 barplot(winningstrat44[3,],
         col=lowcol_main,
         names.arg=c("D","","","","","","","","","","","","","","",""),
         ylim=c(0,800),
         cex.names = 1.5)
 
 #add label
 mtext("Strategies at the leading edge",side=1,line=2,cex=1.5,adj=-1.5)
 
 #plot low cost 8-8
 par(mar=c(4.1,3.21,2.61,0))
 winners_l88<-matrix(NA,2,16)
 winners_l88[1,]<-winningstrat88_stuck[3,]
 winners_l88[2,]<-winningstrat88_crossing[3,]
 barplot(winners_l88,
         col=c(lowcol_light,lowcol_main),
         names.arg=c("","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 
 #plot low cost 1-8
 par(mar=c(4.1,3.1,2.61,1.1))
 winners_l18<-matrix(NA,2,16)
 winners_l18[1,]<-winningstrat18_stuck[3,]
 winners_l18[2,]<-winningstrat18_crossing[3,]
 barplot(winners_l18,
         col=c(lowcol_light,lowcol_main),
         names.arg=c("D","","","","","","","","","","","","","","F","F"),
         ylim=c(0,800),
         cex.names = 1.5)
 legend("topleft",
        legend=c("Advanced","Did not advance"),
        col=c(lowcol_main,lowcol_light),
        pch=15,
        cex=1.5,
        pt.cex=2,
        bty="n"
 )


########################################################################################
 
#### FIGURE 3: Speed and Variation #####################################################
 
# get CVs for each grouping

cv_m<-vector("numeric",4)
cv_m[1]<-sd(subset(results_table$extent,results_table$m_cats == 1))/
  mean(subset(results_table$extent,results_table$m_cats == 1))
cv_m[2]<-sd(subset(results_table$extent,results_table$m_cats == 2))/
  mean(subset(results_table$extent,results_table$m_cats == 2))                  
cv_m[3]<-sd(subset(results_table$extent,results_table$m_cats == 3))/
  mean(subset(results_table$extent,results_table$m_cats == 3)) 
cv_m[4]<-sd(subset(results_table$extent,results_table$m_cats == 4))/
  mean(subset(results_table$extent,results_table$m_cats == 4))

cv_gl<-vector("numeric",4)
cv_gl[1]<-sd(subset(results_table$extent,results_table$gaplength == 0))/
  mean(subset(results_table$extent,results_table$gaplength == 0))
cv_gl[2]<-sd(subset(results_table$extent,results_table$gaplength == 4))/
  mean(subset(results_table$extent,results_table$gaplength == 4))                  
cv_gl[3]<-sd(subset(results_table$extent,results_table$gaplength == 6))/
  mean(subset(results_table$extent,results_table$gaplength == 6)) 
cv_gl[4]<-sd(subset(results_table$extent,results_table$gaplength == 8))/
  mean(subset(results_table$extent,results_table$gaplength == 8))

cv_cost<-vector("numeric",3)
cv_cost[1]<-sd(subset(results_table$extent,results_table$cost_id == 1))/
  mean(subset(results_table$extent,results_table$cost_id == 1))
cv_cost[2]<-sd(subset(results_table$extent,results_table$cost_id == 2))/
  mean(subset(results_table$extent,results_table$cost_id == 2))                  
cv_cost[3]<-sd(subset(results_table$extent,results_table$cost_id == 3))/
  mean(subset(results_table$extent,results_table$cost_id == 3)) 


# plot 3 boxplots, points for cv
layout(matrix(c(1,1,4,2,2,5,3,3,6),3,3,byrow=TRUE))
par(mar=c(2,5,3,1))
boxplot(subset(results_table$extent, results_table$m_cats == 1),
        subset(results_table$extent, results_table$m_cats == 2),
        subset(results_table$extent, results_table$m_cats == 3),
        subset(results_table$extent, results_table$m_cats == 4),
        names=c("1-4","5-8","9-12","13-16"),
        border=c(m1col_main,m2col_main,m3col_main,m4col_main),
        col=c(m1col_light,m2col_light,m3col_light,m4col_light),
        # col=medcol_light,
        cex.axis=1.5)
text(x=4.2,y=280,"A-1",cex=2.5)
mtext("Strat. #",side=1,line=1,adj=-0.05,font=2)

par(mar=c(3,5,2,1))
boxplot(subset(results_table$extent, results_table$gaplength == 0),
        subset(results_table$extent, results_table$gaplength == 4),
        subset(results_table$extent, results_table$gaplength == 6),
        subset(results_table$extent, results_table$gaplength == 8),
        names=c("No gaps","4","6","8"),
        cex.axis=1.5,
        border=c(g1col_main,g2col_main,g3col_main,g4col_main),
        col=c(g1col_light,g2col_light,g3col_light,g4col_light)
        # col=medcol_light
        )
mtext("Invasion extent",side=2,line=3,cex=1.2)
mtext("GL",side=1,line=1,adj=-0.05,font=2)
text(x=4.2,y=280,"B-1",cex=2.5)

par(mar=c(4,5,1,1))
boxplot(subset(results_table$extent, results_table$cost_id == 1),
        subset(results_table$extent, results_table$cost_id == 2),
        subset(results_table$extent, results_table$cost_id == 3),
        names=c("Low","Medium","High"),
        border=c(lowcol_main,medcol_main,highcol_main),
        col=c(lowcol_light,medcol_light,highcol_light),
        # col=medcol_light,
        cex.axis=1.5)
mtext("Cost",side=1,line=1,adj=-0.05,font=2)
text(x=3.3,y=280,"C-1",cex=2.5)

par(mar=c(2,4,3,1))
plot(cv_m,
     ylim=c(0,2.2),
     col=c(m1col_main,m2col_main,m3col_main,m4col_main),
     pch=18,
     xlab="",
     ylab="",
     cex=2.5,
     xaxt="n",
     cex.axis=1.5)
axis(side=1,at=1:4,labels=c("1-4","5-8","9-12","13-16"),cex=1.5)
text(x=1.5,y=2.05,"A-2",cex=2.5)

par(mar=c(3,4,2,1))
plot(cv_gl,
     col=c(g1col_main,g2col_main,g3col_main,g4col_main),
     pch=18,
     ylim=c(0,2.2),
     xlab="",
     ylab="",
     cex=2.5,
     xaxt="n",
     cex.axis=1.5)
axis(side=1,at=1:4,labels=c("No gaps","4","6","8"),cex=1.5)
mtext("CV of extent",side=2,line=3,cex=1.2)
text(x=1.5,y=2.05,"B-2",cex=2.5)

par(mar=c(4,4,1,1))
plot(cv_cost,
     ylim=c(0,2.2),
     pch=18,
     xlab="",
     ylab="",
     col=c(lowcol_main,medcol_main,highcol_main),
     cex=2.5,
     xaxt="n",
     cex.axis=1.5)
axis(side=1,at=1:3,labels=c("Low","Medium","High"),cex=1.5)
text(x=1.3,y=2.05,"C-2",cex=2.5)

###############################################################################

#### Figure 4: Success ########################################################

# create matrices with % successful

#rows of patches
# rows_scs<-matrix(0,2,4)
# 
# for (i in 9001:12000){
#   if (results_table$inv_success[i]==1){
#     x<-results_table$m_cats[i]
#     rows_scs[1,x]<-rows_scs[1,x]+1
#   }
# }
# 
# for (i in 15001:18000){
#   if (results_table$inv_success[i]==1){
#     x<-results_table$m_cats[i]
#     rows_scs[2,x]<-rows_scs[2,x]+1
#   }
# }
# 
# total_mcats<-matrix(0,2,4)
# 
# for (i in 9001:12000){
#   if (results_table$m_cats[i]==1){
#     total_mcats[1,1]<-total_mcats[1,1]+1
#   }
#   else if (results_table$m_cats[i]==2){
#     total_mcats[1,2]<-total_mcats[1,2]+1
#   }
#   else if (results_table$m_cats[i]==3){
#     total_mcats[1,3]<-total_mcats[1,3]+1
#   }
#   else if (results_table$m_cats[i]==4){
#     total_mcats[1,4]<-total_mcats[1,4]+1
#   }
# }
# 
# for (i in 15001:18000){
#   if (results_table$m_cats[i]==1){
#     total_mcats[2,1]<-total_mcats[2,1]+1
#   }
#   else if (results_table$m_cats[i]==2){
#     total_mcats[2,2]<-total_mcats[2,2]+1
#   }
#   else if (results_table$m_cats[i]==3){
#     total_mcats[2,3]<-total_mcats[2,3]+1
#   }
#   else if (results_table$m_cats[i]==4){
#     total_mcats[2,4]<-total_mcats[2,4]+1
#   }
# }
# 
# rows_scs<-rows_scs/total_mcats*100
# 
# #single patches
# single_scs<-matrix(0,2,4)
# 
# for (i in 12001:15000){
#   if (results_table$inv_success[i]==1){
#     x<-results_table$m_cats[i]
#     single_scs[1,x]<-single_scs[1,x]+1
#   }
# }
# 
# for (i in 18001:21000){
#   if (results_table$inv_success[i]==1){
#     x<-results_table$m_cats[i]
#     single_scs[2,x]<-single_scs[2,x]+1
#   }
# }
# 
# total_mcats<-matrix(0,2,4)
# 
# for (i in 12001:15000){
#   if (results_table$m_cats[i]==1){
#     total_mcats[1,1]<-total_mcats[1,1]+1
#   }
#   else if (results_table$m_cats[i]==2){
#     total_mcats[1,2]<-total_mcats[1,2]+1
#   }
#   else if (results_table$m_cats[i]==3){
#     total_mcats[1,3]<-total_mcats[1,3]+1
#   }
#   else if (results_table$m_cats[i]==4){
#     total_mcats[1,4]<-total_mcats[1,4]+1
#   }
# }
# 
# for (i in 18001:21000){
#   if (results_table$m_cats[i]==1){
#     total_mcats[2,1]<-total_mcats[2,1]+1
#   }
#   else if (results_table$m_cats[i]==2){
#     total_mcats[2,2]<-total_mcats[2,2]+1
#   }
#   else if (results_table$m_cats[i]==3){
#     total_mcats[2,3]<-total_mcats[2,3]+1
#   }
#   else if (results_table$m_cats[i]==4){
#     total_mcats[2,4]<-total_mcats[2,4]+1
#   }
# }
# 
# single_scs<-single_scs/total_mcats*100
# 
# #matrix with only the 4 most fecund strategies -- shows
# #percent of invasions successful in 1-6,1-8
# #                                   6-6,8-8
# fecunds<-matrix(NA,2,2)
# fecunds[,1]<-single_scs[,4]
# fecunds[,2]<-rows_scs[,4]
# 
# # plot
# barplot(fecunds,beside=TRUE
#         )
# 
# plot(results_table$extent~)

###############################################################################

#### Figure 4: Seed production ################################################

# Plot seed production curves, K
# needs gNty fn

g1pop<-seq(from=0,length.out = 700,by=1)

klow_01<-(lambda.low[1]-1)/alpha+1
klow_16<-(lambda.low[16]-1)/alpha+1

khigh_01<-(lambda.high[1]-1)/alpha+1
khigh_16<-(lambda.high[16]-1)/alpha+1

seeds_l01<-gNty(g1pop,g1pop,lambda.low[1]) # reaches K at p=463
seeds_l16<-gNty(g1pop,g1pop,lambda.low[16]) # reachs K at p=642
seeds_h01<-gNty(g1pop,g1pop,lambda.high[1])
seeds_h16<-gNty(g1pop,g1pop,lambda.low[16])


par(mfrow=c(2,1))
plot(NA,
     xlim=c(0,673),
     ylim=c(0,700),
     xlab="",
     ylab="",
     main="Low reproductive cost to dispersal"
)
polygon(x=c(g1pop,rev(g1pop)),
        y=c(seeds_l01,rev(seeds_l16)),
        col=lowcol_light,
        border=lowcol_light)
lines(seeds_l01,
      lty=4,
      col=lowcol_main,
      lwd=3)
lines(seeds_l16,
      lty=5,
      col=lowcol_main,
      lwd=3)
abline(h=klow_01,
       lty=4,
       lwd=2)
abline(h=klow_16,
       lty=5,
       lwd=2)
legend(x="bottomright",
       legend=c("Strategy 16 (F)",
                "Strategy 1 (D)"),
       lty=c(5,3),
       lwd=2,
       cex=1.3,
       bty="n")

text(expression("K"["F"]),x=-5,y=650)
text(expression("K"["D"]),x=-5,y=480)

plot(NA,
     xlim=c(0,673),
     ylim=c(0,700),
     xlab="Parent population",
     cex.lab=1.5,
     ylab="",
     main="High reproductive cost to dispersal"
)
polygon(x=c(g1pop,rev(g1pop)),
        y=c(seeds_h01,rev(seeds_h16)),
        col=highcol_light,
        border=highcol_light)
lines(seeds_h01,
      lty=4,
      col=highcol_main,
      lwd=3)
lines(seeds_h16,
      lty=5,
      col=highcol_main,
      lwd=3)
abline(h=khigh_01,
       lty=4,
       lwd=2)
abline(h=khigh_16,
       lty=5,
       lwd=2)
text(expression("K"["F"]),x=-7,y=600)
text(expression("K"["D"]),x=-7,y=150)
mtext("Seeds produced",side=2,line=2,cex=1.5,adj=3)

##################################################################################################

#### Single patches ##############################################################################

allwinners_44<-colSums(winningstrat44)
allwinners_14<-colSums(winning_strat_14)
allwinners_16<-colSums(winningstrat16_crossing)
allwinners_66<-colSums(winningstrat66_crossing)
allwinners_18<-colSums(winningstrat18_crossing)
allwinners_88<-colSums(winningstrat88_crossing)

gap4win<-matrix(NA,2,16)
gap6win<-matrix(NA,2,16)
gap8win<-matrix(NA,2,16)

gap4win[1,]<-allwinners_44
gap4win[2,]<-allwinners_14
gap6win[1,]<-allwinners_66
gap6win[2,]<-allwinners_16
gap8win[1,]<-allwinners_88
gap8win[2,]<-allwinners_18

par(mfrow=c(3,1))
barplot(gap4win,beside=TRUE,
        names.arg=c("D","","","","","","","","","","","","","","","F"),
        cex.names=1.5,
        col=c(m1col_main,m1col_light),
        ylim = c(0,1200))
mtext("gap length: 4",side=1,cex=1.5,line=1)

barplot(gap6win,beside=TRUE,
        names.arg=c("D","","","","","","","","","","","","","","","F"),
        cex.names=1.5,
        col=c(m1col_main,m1col_light),
        ylim = c(0,1200))
mtext("gap length: 6",side=1,cex=1.5,line=1)

barplot(gap8win,beside=TRUE,
        names.arg=c("D","","","","","","","","","","","","","","","F"),
        cex.names=1.5,
        col=c(m1col_main,m1col_light),
        ylim = c(0,1200))
mtext("gap length: 8",side=1,cex=1.5,line=1)
legend("topright",legend=c("Rows of suitable patches","Single patches"),
       pch=15,cex=1.8,
       col=c(m1col_main,m1col_light))
