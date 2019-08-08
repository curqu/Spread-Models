#Trade-off Curves


#Trade-off function
set_curve <- function(c,m,d){   # c is a co-efficient to set maximum lambda
                                # m is the input dispersal parameter
                                # d is the exponent, defines the shape of the curve
                                # k sets the minimum lambda
        lambda<-c*((m)/2)^d
        return(lambda)
}

#Set co-efficient and dispersal strategies
c<-70
m<-c(0.702,0.78,0.86,0.94,1.02,1.1,1.18,1.26,1.34,1.42,1.5,1.58,1.66,1.74,1.82,1.9)
m_line<-c(0.68,0.703,0.705,0.707,0.709,0.71,0.72,0.73,0.75,0.77,0.8,0.83,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2,1.3,1.4,1.5,1.7,1.95,2)

#Trade-off, low cost to dispersal
lambdas03<-set_curve(c,m,0.3)
lambdas03_line<-set_curve(c,m_line,0.3)

lambdas02<-set_curve(c,m,0.25)
lambdas02_line<-set_curve(c,m_line,0.25)


#Trade-off, medium cost to dispersal
lambdas07<-set_curve(c,m,0.6)
lambdas07_line<-set_curve(c,m_line,0.6)

#Trade-off, high cost to dispersal
lambdas15<-set_curve(c,m,1.25)
lambdas15_line<-set_curve(c,m_line,1.25)

#Trade-off, high cost to dispersal
lambdas19<-set_curve(c,m,1.9)
lambdas19_line<-set_curve(c,m_line,1.9)

plot(lambdas15_line~m_line, ylim=c(15,70),xlim=c(0.68,2),main="Trade-off Curves and Strategies Chosen",
     xlab=expression(italic(m)),
     ylab=expression(lambda),type="l",lwd=3,col="#54278f")
lines(x=m_line,y=lambdas03_line,lwd=2,lty=3,col="grey")
lines(x=m_line,y=lambdas19_line,lwd=2,lty=5,col="grey")
lines(x=m_line,y=lambdas07_line,lwd=3,col="#756bb1")
lines(x=m_line,y=lambdas02_line,lwd=3,col="#9e9ac8")
points(x=m,y=lambdas15,pch=16,col="#54278f")
points(x=m,y=lambdas07,pch=16,col="#756bb1")
points(x=m,y=lambdas02,pch=16,col="#9e9ac8")
legend(x="bottomright",
       col=c("#9e9ac8","#756bb1","#54278f","grey","grey","black"), 
       legend=c("Low dispersal cost (d=0.25)","Medium dispersal cost (d=0.7)","High dispersal cost (d=1.5)","d=1.9","d=0.3", "Strategies Used"),
       lty=c(1,1,1,5,3,NA), lwd=c(3,3,3,1,1,NA),
       pch=c(NA,NA,NA,NA,NA,16))

