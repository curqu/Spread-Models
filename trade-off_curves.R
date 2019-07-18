#Trade-off Curves


#Trade-off function
set_curve <- function(c,m,d){   # c is a co-efficient to set maximum lambda
                                # m is the input dispersal parameter
                                # d is the exponent, defines the shape of the curve 
        lambda<-c*((m-0.4)/2)^d
        return(lambda)
}

#Set co-efficient and dispersal strategies
c<-70
m<-c(0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.7,1.95)

#Trade-off, low cost to dispersal
lambdas03<-set_curve(c,m,0.3)


#Trade-off, medium cost to dispersal
lambdas07<-set_curve(c,m,0.7)

#Trade-off, high cost to dispersal
lambdas15<-set_curve(c,m,1.3)


plot(lambdas15~(1/m), ylim=c(0,70),xlim=c(0,2),main="Trade-off Curves and Strategies Chosen",
     xlab="Decline of Probability Density Dispersal Function",
     ylab="Reproductive Rate at Low Density",type="l",lwd=2,col="yellowgreen")
lines(x=1/m,y=lambdas07,lwd=2,col="magenta")
lines(x=1/m,y=lambdas03,lwd=2,col="darkred")
points(x=1/m,y=lambdas15,pch=16,col="yellowgreen")
points(x=1/m,y=lambdas07,pch=16,col="magenta")
points(x=1/m,y=lambdas03,pch=16,col="darkred")
legend(x="bottomright",
       col=c("yellowgreen","darkgreen","blue","darkviolet","magenta","red","black"), 
       legend=c("d=1.5","d=1.2","d=1.1","d=0.9","d=0.7","d=0.5", "Strategies Used"),
       lty=c(1,1,1,1,1,1,NA), pch=c(NA,NA,NA,NA,NA,NA,16))

