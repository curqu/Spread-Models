#Trade-off Curves

#no trade off, only m varies
m<-seq(from=0.25,to=4,by=0.25)
mp<-c(0.5,0.75,1.25,1.75,2.25,2.75,3.25,3.75)
lambdas0<-35

#No trade-off, m and lambda are positively correlated
lambdasp<- 24*m^-0.7

#Trade-off, linear correlation
lambdas1<-70*(m/4)

#Trade-off, non-monotonic
lambdas05<-70*(m/4)^0.5
lambdas05p<-c(24.75,30.31,39.13,46.3,52.5,58.04,63.1,67.78)

#Trade-off, non-monotonic
lambdas07<-70*(m/4)^0.7
lambdas07p<-c(16.33,21.69,31,39.24,46.79,53.85,60.53,66.91)

#Trade-off, non-monotonic
lambdas09<-70*(m/4)^0.9
lambdas09p<-c(10.77,15.52,24.57,33.26,41.71,49.96,58.07,66.05)

#Trade-off, monotonic
lambdas11<-70*(m/4)^1.1
lambdas11p<-c(7.11,11.1,19.47,28.2,37.17,46.36,55.71,65.2)

#Trade-off, monotonic
lambdas12<-70*(m/4)^1.2
lambdas12p<-c(5.77,9.39,17.33,25.96,35.09,44.65,54.56,64.78)

#Trade-off, monotonic
lambdas15<-70*(m/4)^1.5
lambdas15p<-c(3.09,5.68,12.23,20.26,29.53,39.90,51.27,63.54)

plot(lambdas15~m, ylim=c(0,70),main="Trade-off Curves and Strategies Chosen",
     xlab="Decline of Probability Density Dispersal Function",
     ylab="Reproductive Rate at Low Density",type="l",lwd=2,col="yellowgreen")
lines(x=m,y=lambdas12,lwd=2,col="darkgreen")
lines(x=m,y=lambdas11,lwd=2,col="blue")
lines(x=m,y=lambdas09,lwd=2,col="darkviolet")
lines(x=m,y=lambdas07,lwd=2,col="magenta")
lines(x=m,y=lambdas05,lwd=2,col="red")
points(x=mp,y=lambdas15p,pch=16,col="yellowgreen")
points(x=mp,y=lambdas12p,pch=16,col="darkgreen")
points(x=mp,y=lambdas11p,pch=16,col="blue")
points(x=mp,y=lambdas09p,pch=16,col="darkviolet")
points(x=mp,y=lambdas07p,pch=16,col="magenta")
points(x=mp,y=lambdas05p,pch=16,col="red")
legend(x="bottomright",
       col=c("yellowgreen","darkgreen","blue","darkviolet","magenta","red","black"), 
       legend=c("d=1.5","d=1.2","d=1.1","d=0.9","d=0.7","d=0.5", "Strategies Used"),
       lty=c(1,1,1,1,1,1,NA), pch=c(NA,NA,NA,NA,NA,NA,16))

