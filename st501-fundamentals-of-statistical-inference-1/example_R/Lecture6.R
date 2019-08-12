####################################################
##R script corresponding to Video Lecture 6
##Lecture on for loops
##Justin Post
###################################################

######################################################
##Recreating the experiment

#50 people were randomly assigned to two groups - 34 to a group where a 
#person near them yawned (treatment) and 16 to a group where they didn't 
#see someone yawn (control). 10 people in the treatment group and 4 in 
#the control group yawned.

#Create "participants"
people<-1:50
#We have 14 yawners and 36 non-yawners.  Let's say that people 1-14 are yawners
#and people 15-50 aren't

#We can randomly split the 50 people into two groups, one of size
#34 and one of size 16
help(sample)
treatment<-sample(x=people,size=34,replace=FALSE)
#see how many yawners were in the treatment group
trtyawn<-sum(treatment<15)
trtyawn
#that means we have 14-that number of yawners in the control group
controlyawn<-14-sum(treatment<15)
controlyawn

#find the proportions
trtyawn/34
controlyawn/16

#find the difference in proportions
trtyawn/34-controlyawn/16

###################################################
##Repeat process and save values

#vector to store differences
propdiff<-vector()

#Randomly split the 50 people into two groups, one of size 34 and one of size 16
treatment<-sample(x=people,size=34,replace=FALSE)
#find the difference in proportions
propdiff[1]<-sum(treatment<15)/34-(14-sum(treatment<15))/16

#Randomly split the 50 people into two groups, one of size 34 and one of size 16
treatment<-sample(x=people,size=34,replace=FALSE)
#find the difference in proportions
propdiff[2]<-sum(treatment<15)/34-(14-sum(treatment<15))/16

#Randomly split the 50 people into two groups, one of size 34 and one of size 16
treatment<-sample(x=people,size=34,replace=FALSE)
#find the difference in proportions
propdiff[3]<-sum(treatment<15)/34-(14-sum(treatment<15))/16

#....


####################################################
##For loops to automate repeating the process!

#vector to store differences
propdiff<-vector()

#for loop to repeat
for (i in 1:5){
  print(i)
}

for (i in 1:5000){
  #get randomly allocated sample
  treatment<-sample(x=people,size=34,replace=FALSE)

  #find difference in proportions
  propdiff[i]<-sum(treatment<15)/34-(14-sum(treatment<15))/16
}

#plot differences
#check unique values first to get idea for bins
unique(propdiff)
hist(propdiff,main="Histogram of Differences in Proportions",xlab="Difference",breaks=seq(from=-0.55,to=0.55,by=0.01))
#add line for observed difference in mythbusters experiment
abline(v=10/34-4/16,lwd=3,col="Red")

#count number of times that we got a proportion as big or bigger than observed
mean(propdiff>=(10/34-4/16))


