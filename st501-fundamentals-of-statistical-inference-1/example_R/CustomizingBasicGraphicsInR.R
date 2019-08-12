####################################################
##R script corresponding to Video Lecture 3
##Lecture on making graphs beautiful
##Justin Post
###################################################

#look at the built in ToothGrowth data set
head(ToothGrowth)
#inspect data a bit more
str(ToothGrowth)
#attach it so we can call the column names as variables
attach(ToothGrowth)

#################################################################
##Histograms
#For numeric variables, we often make histograms
hist(len)
#look at histogram options
help(hist)

#add a better title
hist(len,main="Histogram of Tooth Length")
#change label on x axis
hist(len,main="Histogram of Tooth Length",xlab="Length Value")
#change label on y axis
hist(len,main="Histogram of Tooth Length",xlab="Length Value",
    ylab="Number in Bin")
#add some shading to the bars
hist(len,main="Histogram of Tooth Length",xlab="Length Value",
     ylab="Number in Bin",density=10)
#change the coloring of the histogram
hist(len,main="Histogram of Tooth Length",xlab="Length Value",
     ylab="Number in Bin",density=10,col="blue")
#change the color of the boxes
hist(len,main="Histogram of Tooth Length",xlab="Length Value",
     ylab="Number in Bin",density=10,col="blue",border="brown")

#we can create two plots in one window
par(mfrow=c(1,2))

#first 30 values are VC, 31-60 are OJ
VClen<-ToothGrowth[1:30,1]
#or VClen<-len[1:30]
OJlen<-ToothGrowth[31:60,1]
#or OJlen<-len[31:60]

#basic histograms
hist(VClen,main="Histogram of VC Lengths",xlab="VC Length")
hist(OJlen,main="Histogram of OJ Lengths",xlab="OJ Length")


####################################################################
##Boxplots
#set plotting window back to 1 plot
par(mfrow=c(1,1))

#create basic boxplot
boxplot(len)
#check options
help(boxplot)
#create boxplots for OJ and VC
boxplot(len~supp)
#add title
boxplot(len~supp,main="Boxplots of Lengths by Supplement")
#change labeling
boxplot(len~supp,main="Boxplots of Lengths by Supplement",
        names=c("OJ values","VJ values"))
#make them horizontal 
boxplot(len~supp,main="Boxplots of Lengths by Supplement",
        names=c("OJ values","VJ values"),horizontal=TRUE)

#get summary of boxplot without plotting
boxplot(len~supp,main="Boxplots of Lengths by Supplement",
        names=c("OJ values","VJ values"),plot=FALSE)
#save this as an R object
box<-boxplot(len~supp,main="Boxplots of Lengths by Supplement",
        names=c("OJ values","VJ values"),plot=FALSE)
box
#access the attributes of the object using the $ operator
box$
box$names
box$stats


#############################################################
##Scatterplots
#create a scatterplot of the dose and length variables
plot(len,dose)
#switch x and y
plot(x=dose,y=len)
#look at options
help(plot)
#change title and labels
plot(x=dose,y=len,main="Scatterplot of Length and Dose",xlab="Dose",ylab="Length")

#change some options with par 
plot(x=dose,y=len,main="Scatterplot of Length and Dose",xlab="Dose",ylab="Length",
     pch=5)
plot(x=dose,y=len,main="Scatterplot of Length and Dose",xlab="Dose",ylab="Length",
     pch=5,cex=0.5)
#add lines to the plot corresponding to the overall mean of length
abline(h=mean(len))
#change thickness
abline(h=mean(len),lwd=3)
#change color
abline(h=mean(len),lwd=3,col="blue")

#plot points and change color depending on supplement
#create color vector corresponding to supplement
colorvec<-c(rep("Blue",30),rep("Green",30))
colorvec
plot(x=dose,y=len,main="Scatterplot of Length and Dose",xlab="Dose",ylab="Length",
     pch=15,cex=0.5,col=colorvec)

