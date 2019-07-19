#Trade-off Curves


#Trade-off function
set_curve <- function(c,m,d,k){   # c is a co-efficient to set maximum lambda
                                # m is the input dispersal parameter
                                # d is the exponent, defines the shape of the curve
                                # k sets the minimum lambda
        lambda<-c*((m-0.7)/2)^d+k
        return(lambda)
}

#Set co-efficient and dispersal strategies
c<-60
k<-10
m<-c(0.71,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.7,1.95)
m_line<-c(0.7,0.703,0.705,0.707,0.709,0.71,0.72,0.73,0.75,0.77,0.8,0.83,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.7,1.95,2)

#Trade-off, low cost to dispersal
lambdas03<-set_curve(c,m,0.3,k)
lambdas03_line<-set_curve(c,m_line,0.3,k)


#Trade-off, medium cost to dispersal
lambdas07<-set_curve(c,m,0.7,k)
lambdas07_line<-set_curve(c,m_line,0.7,k)

#Trade-off, high cost to dispersal
lambdas15<-set_curve(c,m,1.5,k)
lambdas15_line<-set_curve(c,m_line,1.5,k)

plot(lambdas15_line~m_line, ylim=c(0,70),xlim=c(0.5,2),main="Trade-off Curves and Strategies Chosen",
     xlab=expression(italic(m)),
     ylab=expression(lambda),type="l",lwd=2,col="yellowgreen")
lines(x=m_line,y=lambdas07_line,lwd=2,col="magenta")
lines(x=m_line,y=lambdas03_line,lwd=2,col="darkred")
points(x=m,y=lambdas15,pch=16,col="yellowgreen")
points(x=m,y=lambdas07,pch=16,col="magenta")
points(x=m,y=lambdas03,pch=16,col="darkred")
legend(x="bottomright",
       col=c("yellowgreen","magenta","darkred","black"), 
       legend=c("d=1.5","d=0.7","d=0.3", "Strategies Used"),
       lty=c(1,1,1,NA), pch=c(NA,NA,NA,16))

