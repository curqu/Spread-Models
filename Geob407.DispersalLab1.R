#Geob 407. #Dispersal Lab
#Spring 2019

# I have given you some of the code, but the rest you will need to write yourself.
# You can do it!

# Forgot how to plot something? Look at old labs

#Part 3. Enter the data (separated by commas).
#Step 3. Enter the data
distance<-c(1, 2, 4, 5, 6, 10)
seeds<-c(39,20,1,2,2,1)

#Step 4.Plot your data. 
# Number of seeds as a function of distance.
# Give it a title (e.g. runway #25)
# Label the axes appropriately, with units
# Choose a solid option for the points
# Quick R will remind you how: https://www.statmethods.net/advgraphs/parameters.html


#Step 5
#calculate the proportion of seeds at each distance
prop.seeds<-seeds/sum(seeds) 

#Plot the the transformed data (proportion seeds dispersed as a function of distance)
plot(distance,prop.seeds)

#How do your two plots compare?
#same as the first, right? convince yourself using the back arrows

#log transform your proportion data
log.prop.seeds<-log(prop.seeds) 

#make another plot (log (proportion seeds dispersed as a function of distance)). Give it a title
plot(distance,log.prop.seeds)
  
#these are the data you are going to fit the line through
#You also need this plot for your assignment once you add the line in Step 7.

#use a linear model to fit a line through your data
fitkernel<- lm(log.prop.seeds~distance)

#check out the results of the linear model
# We are most interested in the coefficients (even if the model fit is bad,
# or the coefficents are not significant).
# You are looking for the slope of the line. What you will see are coefficients for:
# (Intercept) & distance. The coeffience for distance is the slope.
summary(fitkernel)

#the slope is ALSO an estimate of the rate parameter of the negative exponential distribution.
# the estimate you get is negative. the rate parameter will be positive
# that is, you need to take the absolute value of the coefficent you get.
#**NOTE: if you are interested, code for calculating m using a maximum likelihood approach to fit the negative exponential distribution to your data is at the end of the script.

#Step 6
#Record your estimate of the rate parameter (m) and your calculation of the 
#mean dispersal distance (1/m) in your notes. Did you read the comments above?
#should m be positive or negative?

#Use R to do the calculation of mean dispersal distance
mean.default(distance)

#Step 7
#Draw a line through your points based on the linear model
#to see the coefficients:
coef(fitkernel)

# we are going to use the command abline to draw a line in the last plot you made
# it needs an intercept and slope. 
# we can get those by calling them from the coefficients (terms 1 & 2)
abline(a = coef(fitkernel)[1], b=coef(fitkernel)[2], lwd=2, col="blue")

#Step 8. Make a new plot with the fitted kernel & the raw data (seeds as a function of distance)
#Give it a title (fill in quotes after main)
plot(distance ~ seeds)

#Hint: use the same code you did to make the first plot

#Now, create a sequence of distances from 0.9 to the maximum distance, using increments of 0.1
y<-seq(0.9,10,0.1)

#calculate the total number of seeds in your runway
totalseeds<-sum(seeds)

#draw a line of the fitted kernel
lines(y,totalseeds*dexp(y,rate=-coef(fitkernel)[2]), lwd=2,lty=2, col="green3")

#Step 9. Record your data on the board.

#If you are finished early, help another student!

###*************************************************************************************
##Take a break! Help another student! 
##We will start again when all data are collected & analyzed
###*************************************************************************************

#Part 4. Analyze the class data.

#Step 10. Read in the data
classdata<-read.csv('Geob407.Dispersal.MASTERseeddata.UPDATE.csv')

#R can't find the data?
#Make sure you've changed your working directory AND that the file is in that directory

#look at the dataframe
str(classdata)

#Step 11.
#Make 4 plots to evaluate the predictions you made in Part 1
#Specify the limits of the y axis so the plots are easier to compare.
#E.g. ylim = c(0,25) for max d
x <- classdata$distance
h<-hist(x, col='blue', xlab = "Dispersal Distance", main="Dispersal Distance Frequency with Fitted Curve")
xfit<-seq(min(x),max(x))
#yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#yfit<-yfit*diff(h$mids[1:2])*length(x)
#lines(xfit,yfit,col="red", lwd=2)
y<-seq(0,21,0.1)
totalseeds<-sum(classdata$seeds)
fitkernel<- lm(log.prop.height~distance)

lines(y,totalseeds*dexp(y,rate=-coef(fitkernel)[2]), lwd=2,lty=2, col="green3")

#Kernel density plot
d<- density(classdata$distance)
plot(d, main="Dispersal Kernel")
polygon(d, col="red", border="black")

#multiple kernels by plant height
#seperate height into quartiles
heightq1<-classdata[ which(classdata$height <= 28.0), ]
heightq2<-classdata[ which(classdata$height  > 28.0 & classdata$height <= 31.0), ]
heightq3<-classdata[ which(classdata$height > 31.0 & classdata$height < 34.0), ]
heightq4<-classdata[ which(classdata$height >= 34.0), ]
heightq1$hquartile<-1
heightq2$hquartile<-2
heightq3$hquartile<-3
heightq4$hquartile<-4
rbind(heightq1,heightq2,heightq3,heightq4)

#show dispersal by height quartiles
mdd1<-mean(heightq1$distance)
mdd2<-mean(heightq2$distance)
mdd3<-mean(heightq3$distance)
mdd4<-mean(heightq4$distance)
mdd<-c(mdd1,mdd2,mdd3,mdd4)

#mean height of each quartile 
mh1<-mean(heightq1$height)
mh2<-mean(heightq2$height)
mh3<-mean(heightq3$height)
mh4<-mean(heightq4$height)
mh<-c(mh1,mh2,mh3,mh4)
model2<-lm(mdd~mh)
plot(mdd~mh, xlab="Mean Height of Quartile", ylab="Mean Dispersal Distance")
abline(model2)

 
#build kernels compare by height
library(sm)
height.f<-factor(classdata$height, levels=25)
sm.density.compare(classdata$distance, classdata$height)
sm.density.compare(classdata$distance, classdata$height, xlab="Dispersal Distance Kernels by Height")


d1<-density(heightq1$distance)
d2<-density(heightq2$distance)
d3<-density(heightq3$distance)
d4<-density(heightq4$distance)

plot(d2, main="Dispersal Kernels by Height")
lines(d4)
lines(d3)
lines(d1)

#Make the 4 plots in a matrix (2 rows & 2 columns)
par(mfrow=c(2,2)) 

#if you want to go back to just one plot. You will need to give it a new par command
#which is par(mfrow=c(1,1)) OR click on the icon with the red x in the plot window.
#this will delete the current plot AND reset the parameters for plotting

#hint: what follows should be the code to make 4 plots. Label the x & y axes and give each plot a short title.



#Step 12.
# Use linear models to test whether or not any of the relationships are significant

# How do you tell whether or not it's significant?
# For the purposes of this class, we are interested the p-value & the R-square
# (Note that this is not enough for proper reporting in a journal article)
# Is P < 0.05?
# Remember the R-square (focus on the adjusted R-square) tells you
# how much variation is explained by the explanatory variable.

#name each model you build (e.g. test1 <- or model1<-)

#height ~ distance
model1<-lm(distance ~ height, data=classdata)
plot(model1)
plot(classdata$distance ~ classdata$height, xlab="Plant Height", ylab="Dispersal Distance", main = "Dispersal Distance by Height")
abline(model1)

model3<-lm(classdata$distance ~ classdata$seeds)
plot(classdata$distance ~ classdata$seeds, xlab="Seed Count", ylab="Dispersal Distance", main="Dispersal as a function of seed count")
abline(model3)

model4<-lm(distance ~ height + siliques, data=classdata)


#if you want to draw a line to show significant relationships
#use the command abline (see code in step 7)


###*************************************************************************************
##OPTIONAL
##If you want to check your linear approximate for m 
## and calculate me from the negative exponential
###*************************************************************************************
install.packages("bbmle") #you only need to do this once
library(bbmle)

#function for negative log-likelihood (needed to fit kernels)
fitNLLnegexp<-function(m){
    NLL<- -sum(dexp(dist,m, log=T))
    return(NLL)
}

#make a vector of distances
#this means each seed will get a distance
#so if 8 seeds went 1 cm. you will get a string of 8 1's
attach(classdata)
for (j in 1:length(distance))
{
    if(j==1){
        distance.vec<-rep(distance[1],seeds[1])
    }
    else{
        distance.vec<-c(distance.vec,rep(distance[j],seeds[j]))
    }}

#use maximum likelihood to find best estimate for rate parameter (m)
fit1<-mle2(minuslogl=fitNLLnegexp,start=list(m=0.2), data=list(dist=distance.vec), method="BFGS")  
#you may get some warnings. don't worry about them.

#now look at the results
summary(fit1)

#you should get the same m (or nearly)








