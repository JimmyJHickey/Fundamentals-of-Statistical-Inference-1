################################################################################
##R script to display CLT
##Justin Post - 2017
################################################################################

#change plotting window
par(mfrow=c(1,3))

#Simulate 10000 samples of size 3,10,50 from N(mu,sigma) distribution
n<-c(3,10,50)
N<-50000
mu<-10
sigma<-3

#list to save data values in (not an efficient way to do this!)
data<-list()
for(i in 1:length(n)){
  data[[i]]<-matrix(0,nrow=N,ncol=n[i])
}

#Create the data
#loop over sample sizes
for (j in 1:length(n)){
  #loop over data sets
  for (i in 1:N){
    data[[j]][i,]<-rnorm(n=n[j],mean=mu,sd=sigma)
	}
}

#calculate the z statistic for each sample
means3<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
means10<-apply(X=data[[2]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[2]))},MARGIN=1)
means50<-apply(X=data[[3]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[3]))},MARGIN=1)

hist(means3,main=paste("Histogram of z's with n=",n[1]," from N(",mu,",",sigma^2,")",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means10,main=paste("Histogram of z's with n=",n[2]," from N(",mu,",",sigma^2,")",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means50,main=paste("Histogram of z's with n=",n[3]," from N(",mu,",",sigma^2,")",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))


#############################################################
##repeat but with uniform(0,1)
#Simulate 10000 samples of size 3,10,50 from U(0,1) distribution
n<-c(3,10,50)
N<-50000
mu<-1/2
sigma<-sqrt((1-0)^2/12)

#list to save data values in
data<-list()
for(i in 1:length(n)){
  data[[i]]<-matrix(0,nrow=N,ncol=n[i])
}

#Create the data
#loop over sample sizes
for (j in 1:length(n)){
  #loop over data sets
  for (i in 1:N){
    data[[j]][i,]<-runif(n=n[j])
  }
}

#calculate the z statistic for each sample
means3<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
means10<-apply(X=data[[2]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[2]))},MARGIN=1)
means50<-apply(X=data[[3]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[3]))},MARGIN=1)

hist(means3,main=paste("Histogram of z's with n=",n[1]," from U(0,1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means10,main=paste("Histogram of z's with n=",n[2]," from U(0,1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means50,main=paste("Histogram of z's with n=",n[3]," from U(0,1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))



#############################################################
##repeat but with exp(1)
#Simulate 10000 samples of size 3,10,50 from U(0,1) distribution
n<-c(3,10,50)
N<-50000
mu<-1
sigma<-1

#list to save data values in
data<-list()
for(i in 1:length(n)){
  data[[i]]<-matrix(0,nrow=N,ncol=n[i])
}

#Create the data
#loop over sample sizes
for (j in 1:length(n)){
  #loop over data sets
  for (i in 1:N){
    data[[j]][i,]<-rexp(n=n[j])
  }
}

#calculate the z statistic for each sample
means3<-apply(X=data[[1]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[1]))},MARGIN=1)
means10<-apply(X=data[[2]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[2]))},MARGIN=1)
means50<-apply(X=data[[3]],FUN=function(data){(mean(data)-mu)/(sigma/sqrt(n[3]))},MARGIN=1)

hist(means3,main=paste("Histogram of z's with n=",n[1]," from exp(1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means10,main=paste("Histogram of z's with n=",n[2]," from exp(1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))
hist(means50,main=paste("Histogram of z's with n=",n[3]," from exp(1)",sep=""),prob=T)
lines(seq(from=-3,to=3,by=0.01),dnorm(seq(from=-3,to=3,by=0.01)))


