#######################################################################
##Example showing convergence in distribution to a constant as well as 
##convergence in distribution does not imply convergence in probability
##Finally, convergence in distribution
##Justin Post - 2017
#######################################################################

#######################################################################
##Convergence in distribution to a constant

#Generate many samples of size n from Uniform(0,1), find max for each sample to see the distribution of max
n<-c(3,10,100,10000)
N<-20000
maxes<-matrix(,nrow=N,ncol=length(n))

#loop over sample sizes (not efficient way to do this!)
for (j in 1:length(n)){
	#loop through data sets
	for (i in 1:N){
		maxes[i,j]<-max(runif(n[j]))
	}
}

par(mfrow=c(1,length(n)))
hist(maxes[,1],breaks=seq(0,1,by=0.005),main=paste("Hist of Max, n=",n[1],sep=""),xlab="max",ylab="Dist of max")
hist(maxes[,2],breaks=seq(0,1,by=0.005),main=paste("Hist of Max, n=",n[2],sep=""),xlab="max",ylab="Dist of max")
hist(maxes[,3],breaks=seq(0,1,by=0.005),main=paste("Hist of Max, n=",n[3],sep=""),xlab="max",ylab="Dist of max")
hist(maxes[,4],breaks=seq(0,1,by=0.005),main=paste("Hist of Max, n=",n[4],sep=""),xlab="max",ylab="Dist of max")


#######################################################################
#######################################################################
###Convergence in distribution but not in probability

#Consider an example where X~Beta(2,2) then 1-X will also be Beta(2,2)
#Define a sequence of RVs to be X_n=X for all n

#X_n converges in distribution to 1-X~Beta(2,2)

#This distribution looks like
plot(x=seq(from=0,to=1,by=0.01),y=dbeta(seq(from=0,to=1,by=0.01),2,2),type="l",main="Distribution of X_n for all n",xlab="x",ylab="f_X_n")


###########################
#Now consider convergence in probability - i.e. does X_n converge in probability to 1-X?
#We can show that X_n does not converge in probability to 1-X.
#To converge in probability we need P(|X_n-(1-X)|<e)=P(|X+X_n-1|<e) -> 1 as n goes to infinity for every e>0

#But X_n is defined as X -->  = P(|2X-1|<e) = P((1-e)/2 < X < (1+e)/2) < 1 for e=1/2
#= P(1/4 < X < 3/4) <1 for every n.

#For a visual display of this, we generate many values from a beta(2,2)
value<-rbeta(100000,2,2)
par(mfrow=c(1,2))
plot(x=seq(from=0,to=1,by=0.01),y=dbeta(seq(from=0,to=1,by=0.01),2,2),type="l",main="Distribution of X_n for all n",xlab="x",ylab="f_X_n")
#Now we find |X-(1-X)| and plot this
hist(abs(value-(1-value)),main="Hist of |X_n-(1-X)| values",xlab="|x_n-(1-x)|")

#Notice that this probability is not tending toward 1 for any 0<e<1 (in fact it doesn't depend on n at all).  
#Therefore, these RVs do not converge in probability! (even though they converge in distribution!)



#######################################################################
#######################################################################
##Convergence in distribution (of w=n(1-max) from U(0,1) to the exp(1))

#Generate many samples of size n from Uniform(0,1), find max for each sample to see the distribution of w=n(1-max)
n<-c(3,10,100,10000)
N<-20000
w<-matrix(,nrow=N,ncol=length(n))

#loop over sample sizes
for (j in 1:length(n)){
	#loop through data sets
	for (i in 1:N){
		w[i,j]<-n[j]*(1-max(runif(n[j])))
	}
}

par(mfrow=c(1,length(n)))
hist(w[,1],main=paste("Hist of W, n=",n[1],sep=""),xlab="max",ylab="Dist of W",prob=T)
seq<-seq(from=0,to=10,by=0.01)
lines(seq,y=dexp(seq))
hist(w[,2],main=paste("Hist of W, n=",n[2],sep=""),xlab="max",ylab="Dist of W",prob=T)
lines(seq,y=dexp(seq))
hist(w[,3],main=paste("Hist of W, n=",n[3],sep=""),xlab="max",ylab="Dist of W",prob=T)
lines(seq,y=dexp(seq))
hist(w[,4],main=paste("Hist of W, n=",n[4],sep=""),xlab="max",ylab="Dist of W",prob=T)
lines(seq,y=dexp(seq))





