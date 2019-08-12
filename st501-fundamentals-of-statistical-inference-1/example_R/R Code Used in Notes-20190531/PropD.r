##########################################################################
##Fun with Prop D from section 2.3
##Author: Justin Post - 2017
##  Jimmy Hickey - 2019-06-19
##########################################################################

##########################################################################
#Practice generating Uniform
##########################################################################

#set seed so we can replicate our findings
set.seed(9235)                            
#Store the numbers to graph
# Generate 50000 random draws between (0, 1)
n<-50000
u<-runif(n=n,min=0,max=1)

#Create a histogram to check that this matches the pdf
hist(u,main="Histogram of 50000 Random U(0,1) Values",freq=FALSE)
abline(h=1,lwd=2)
#histogram looks roughly like a uniform distribution!

##########################################################################
#Exponential Example
##########################################################################

#Inverse of the exponential cdf is given by -2ln(1-u) where ln is the natural log (log() in R defaults to the natural log)
#So this isn't too bad, to define our Y we just run the code
Y<- -(1/2)*log(1-u)

#Check that the pdf estimate matches up
hist(Y, breaks=seq(from=0,to=6.5,by=0.25),main="Empirical distribution (estimate of pdf) of Y",freq=FALSE)
#overlay true distribution
curve(2*exp(-2*x),from=0,to=6.5,add=TRUE,lwd=2)

#Check the cdf matches up
plot(ecdf(Y),ylab="CDF estimate",xlab="y",main="CDF estimate for Exponential Variable",xlim=c(0,10),lwd=2)
#overlay true CDF
curve(1-exp(-2*x),from=0,to=10,lwd=2,lty=2,col="Blue",add=TRUE)

#We can also use these random draws to estimate attributes from the distribution
#For example, we can find the approximate probability that an Exp(2) RV is greater than 3
sum(Y>3)/length(Y)
#Compare this to the exact probability
1-pexp(q=3,rate=2)

#We can also estimate the any quantile, say we want Q1, Median, and Q3 
round(quantile(Y),3)

#compare this to the exact Q1, M, and Q3
c(-(1/2)*log(1-0.25),-(1/2)*log(1-0.5),-(1/2)*log(1-0.75))

#estimate the mean
mean(Y)
#Truth is 1/2

##########################################################################
#Poisson Example
##########################################################################

#We can get the cdf values for the Poisson using the function ppois
ppois(q=0,lambda=4)
ppois(q=1,lambda=4)
ppois(q=3,lambda=4)
ppois(q=4,lambda=4)
ppois(q=5,lambda=4)
ppois(q=6,lambda=4)
ppois(q=7,lambda=4)

#Since we have random numbers between 0 and 1, how can we use prop D to generate random Poisson values?
#We'll can create a corresponding inverse cdf!

#If u is a number less than ppois(q=0,lambda=4) assign it a 0
ppois(q=0,lambda=4)
#If u is a number between ppois(q=0,lambda=4) and ppois(q=1,lambda=4) assign it a 1
ppois(q=0,lambda=4)
ppois(q=1,lambda=4)
#If u is a number between ppois(q=1,lambda=4) and ppois(q=2,lambda=4) assign it a 2
ppois(q=1,lambda=4)
ppois(q=2,lambda=4)
#... and so on!

#We'll write a little function to take in a random uniform value and output the value of Y, for simplicity we'll lump all vaues together after y=8
#note - in R, & means "and"
Poifunction<-function(u){
	if(u<ppois(q=0,lambda=4)){
		return(0)
	}else if((ppois(q=0,lambda=4)<u)&(u<ppois(q=1,lambda=4))){
		return(1)
	}else if((ppois(q=1,lambda=4)<u)&(u<ppois(q=2,lambda=4))){
		return(2)
	}else if((ppois(q=2,lambda=4)<u)&(u<ppois(q=3,lambda=4))){
		return(3)
	}else if((ppois(q=3,lambda=4)<u)&(u<ppois(q=4,lambda=4))){
		return(4)
	}else if((ppois(q=4,lambda=4)<u)&(u<ppois(q=5,lambda=4))){
		return(5)
	}else if((ppois(q=5,lambda=4)<u)&(u<ppois(q=6,lambda=4))){
		return(6)
	}else if((ppois(q=6,lambda=4)<u)&(u<ppois(q=7,lambda=4))){
		return(7)
	}else{
		return(8)
  }
}

#Now we'll use the apply function to test every value in the u vector.  (apply must take in a matrix not a vector, so 
#we use the function as.matrix to turn u from a vector of length 50000 to a matrix of dimension 50000 by 1
Y<-apply(X=as.matrix(u),FUN=Poifunction,MARGIN=1)

#let's plot the pmf and cdf to check
hist(Y, breaks=seq(from=-0.5,to=8.5,by=0.5),main="Empirical distribution (estimate of pmf) of Y",freq=FALSE)

#CDF plot
plot(ecdf(Y),xlab="y",ylab="CDF estimate",main="CDF estimate for Poisson")

#Estimate P(Y<3)=P(Y<=2)
sum(Y<3)/n
#Truth
ppois(q=2,lambda=4)

#Estimate E(Y)
mean(Y)
#Truth = lambda=4

#Estimate Q1, M, Q3
quantile(Y)
qpois(c(0.25,0.5,0.75),lambda=4)

