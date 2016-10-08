xlim=c(0,25)
xpot=c(8,15)
a<-1
b<-1
sinus1<-function(x){a*sin(b*x)}
c<-sinus1(xpot[1])
d<-0.35
expo <-function(x){c*exp(-d*(x-xpot[1]))}
e<-expo(xpot[2])
f<-1
sinus2<-function(x){e*cos(f*(x-xpot[2]))}
line0<-function(x){0*x/x}
line1<-function(x){1.5*x/x}
linevup=function(x){1.5*(x-xpot[1]+0.00001)*50000}
linevdown=function(x){1.5*(x-xpot[2]+0.00001)*50000}
yaxis=function(x){-1+2.5*(x+1+0.000001)*500000}

plot(NA,xlim=xlim+c(-0.5,0),ylim=c(-1,1.5),ylab="",xlab="",bty="n",xaxt="n",yaxt="n")
curve(sinus1,xlim[1],xpot[1],add=TRUE,col="red",n=10001,lwd=3)
curve(expo,xpot[1],xpot[2],add=TRUE,col="red",n=10001,lwd=3)
curve(sinus2,xpot[2],xlim[2],add=TRUE,col="red",n=10001,lwd=3)
curve(line0,xlim[1],xpot[1],add=TRUE,col="blue",n=10001,lwd=3)
curve(line1,xpot[1],xpot[2],add=TRUE,col="blue",n=10001,lwd=3)
curve(line0,xpot[2],xlim[2],add=TRUE,col="blue",n=10001,lwd=3)
curve(linevup,xpot[1]-0.00001,xpot[1]+0.00001,add=TRUE,col="blue",n=10001,lwd=3)
curve(linevdown,xpot[2]-0.00001,xpot[2]+0.00001,add=TRUE,col="blue",n=10001,lwd=3)

curve(line0,xlim[1]-1,xlim[2]+1,add=TRUE,col="black",n=10001,lwd=1)
curve(yaxis,-1-0.000001,-1+0.000001,add=TRUE,col="black",n=10001,lwd=1)
