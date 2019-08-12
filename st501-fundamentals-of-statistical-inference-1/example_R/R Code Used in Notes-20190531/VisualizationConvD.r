################################################################################
##R script to introduce convergence in distribution
##Justin Post - 2017
################################################################################

#Simulate N samples of size n from an exp(1) distribution
n<-c(3,30,300)
N<-50000

#list to save data values in
data<-list()
for(i in 1:length(n)){ 	data[[i]]<-matrix(0,nrow=N,ncol=n[i]) }

#Create the data - loop over sample sizes
for (j in 1:length(n)){	#loop over data sets
	for (i in 1:N){ 	
		data[[j]][i,]<-rexp(n=n[j])	
	} 
}

#mean and variance for exp(1)
mu<-sigma<-1

par(mfrow=c(1,3))

#calculate the z statistic for each set of samples
means1<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
hist(means1,main=paste("Histogram of z's with n=",n[1]," from exp(1)",sep=""),xlab="Means",prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))

means2<-apply(X=data[[2]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[2]))},MARGIN=1)
hist(means2,main=paste("Histogram of z's with n=",n[2]," from exp(1)",sep=""),xlab="Means",prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))

means3<-apply(X=data[[3]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[3]))},MARGIN=1)
hist(means3,main=paste("Histogram of z's with n=",n[3]," from exp(1)",sep=""),xlab="Means",prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
