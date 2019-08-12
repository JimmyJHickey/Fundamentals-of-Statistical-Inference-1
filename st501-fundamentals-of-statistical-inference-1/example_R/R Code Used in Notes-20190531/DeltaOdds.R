########################################################################
##R file to display delta method for odds of success from ber(p)
##Justin Post - 2017
########################################################################

#change plotting window so we can compare values
par(mfrow=c(3,1))

#function to get 1st order taylor expansion about mu_ybar=p 
taylor<-function(p,ybar){
	p/(1-p)+(1/(1-p)^2)*(ybar-p)
}

#plot of function ybar/(1-ybar)
ybar<-seq(from=0.01,to=0.96,by=0.01)
plot(x=ybar,y=ybar/(1-ybar),xlab="ybar",ylab="g(ybar)=ybar/(1-ybar)",main="Plot of ybar/(1-ybar)",type="l",lwd="2")

#do p=0.2
p = 0.2
lines(x=ybar,y=taylor(p,ybar),lwd="2",col="blue")

#add in distribution of ybar for n=100
n<-100

#y-bar is asymptotically normal(p,p(1-p)/n) (we'll learn this when we do the CLT)
lines(x=ybar,y=2*dnorm(ybar,mean=p,sd=sqrt(p*(1-p)/n)),type="l",lwd="2",col="Orange")
text("p=0.2, n=100",x=0.5,y=70)


#repeat for p=0.5

plot(x=ybar,y=ybar/(1-ybar),xlab="ybar",ylab="g(ybar)=ybar/(1-ybar)",main="Plot of ybar/(1-ybar)",type="l",lwd="2")

#do p=0.5
p = 0.5
lines(x=ybar,y=taylor(p,ybar),lwd="2",col="blue")

#add in distribution of ybar for n=100
n<-100

#y-bar is asymptotically normal(p,p(1-p)/n) (we'll learn this when we do the CLT)
lines(x=ybar,y=2*dnorm(ybar,mean=p,sd=sqrt(p*(1-p)/n)),type="l",lwd="2",col="Orange")
text("p=0.5, n=100",x=0.3,y=70)


#repeat for p=0.8
plot(x=ybar,y=ybar/(1-ybar),xlab="ybar",ylab="g(ybar)=ybar/(1-ybar)",main="Plot of ybar/(1-ybar)",type="l",lwd="2")

#do p=0.8
p = 0.8
lines(x=ybar,y=taylor(p,ybar),lwd="2",col="blue")

#add in distribution of ybar for n=100
n<-100

#y-bar is asymptotically normal(p,p(1-p)/n) (we'll learn this when we do the CLT)
lines(x=ybar,y=2*dnorm(ybar,mean=p,sd=sqrt(p*(1-p)/n)),type="l",lwd="2",col="Orange")
text("p=0.8, n=100",x=0.3,y=70)



#Give approximate expected values using simulation and compare to delta approx
#number of data sets to execute
N<-100000
data<-rbinom(N,size=100,prob=0.2)
avg<-data/100
mean(avg/(1-avg))

data<-rbinom(N,size=100,prob=0.5)
avg<-data/100
mean(avg/(1-avg))

data<-rbinom(N,size=100,prob=0.8)
avg<-data/100
mean(avg/(1-avg))
