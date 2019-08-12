###########################################################################
##Program to demonstrate convergence in prob
##This is really just showing the WLLN
##Justin Post - 2017
###########################################################################

#Generate this many datasets
N<-10000
#dummy vectors to store things
vars<-means<-rep(0,N)
#create data set of size i from normal, save mean/var of each sample 
for (i in 1:N){    
	datavalues<-rnorm(n=i)
        means[i]<-mean(datavalues)
        vars[i]<-var(datavalues) 
}
plot(x=1:N,y=means,main="Plot of Sample Mean Observations",xlab="Number of data values",ylab="Value of Mean",cex=0.1)
abline(h=0)
plot(x=1:N,y=vars,main="Plot of Sample Variance Observations",xlab="Number of data values",ylab="Value of Var",cex=0.1)
abline(h=1)
