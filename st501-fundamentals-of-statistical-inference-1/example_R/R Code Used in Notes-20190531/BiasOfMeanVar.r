###################################################################
###Script to show the unbiased nature of Y-bar and the biased nature 
###of S^2=(1/n)sum(Yi-Ybar)^2 from a RS
###Justin Post 2017
###################################################################

#set seed for random number generation
set.seed(121)

##################################################################
#Data creation
##################################################################
#Generate n random variables from normal, a total of N datasets
#sample size
n<-10
#number of data sets
N<-10000

#create dummy vectors to hold values
meanx<-varx<-rep(0,N)

#Loop through and create the normal data
for (i in 1:N){
	x<-rnorm(n)
	#find and save mean for each sample
	meanx[i]<-mean(x)
	#find and save variance for each sample
	varx[i]<-(1/n)*sum((x-mean(x))^2)
}

##################################################################
#Plotting
##################################################################
#set plot window to have 2 columns and 1 row
par(mfrow=c(1,2))

#Plot normal sample mean values and sample variance values

#mean
hist(meanx,main=paste("Histogram of Normal(0,1) Sample Means\n"," of size n=",n,"N=",N,"total samples"))
#add in text to the plot
text(x=0.75,y=2000,paste("True Mean is 0\n Observed mean of sample\n means is ",round(mean(meanx),3)))

#variance
hist(varx,main=paste("Histogram of Normals(0,1) (Biased) Sample Variances\n"," of size n=",n,"N=",N,"total samples"))
#add in text to the plot
text(x=2,y=1500,paste("True Variance is 1\n Observed mean of sample \n variances is ",round(mean(varx),4)))

#Note, the unbiased version of S^2 multiples the biased version by (n/(n-1))
(n/(n-1))*0.8981


