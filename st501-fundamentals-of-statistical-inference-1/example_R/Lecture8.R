####################################################
##R script corresponding to Video Lecture 8
##Lecture on apply functions and object output
##Justin Post
###################################################

######################################################
#read in scores data
data<-read.csv("scores.csv",header=TRUE)
attach(data)

#Get just away score columns
AScores<-data[,c(6,7,8,9,12)]
head(AScores)

#Get mean of each column, use apply function
help(apply)

#apply mean to columns
apply(X=AScores,FUN=mean,MARGIN=2)

#apply coefficient of variation and average of max and min functions
coefVar<-function(data){
  return(sd(data)/abs(mean(data)))
}

avgRange<-function(data){
  return((min(data)+max(data))/2)
}

#use apply across columns
apply(X=AScores,FUN=coefVar,MARGIN=2)
apply(X=AScores,FUN=avgRange,MARGIN=2)

#apply a function to every element of a list with lapply
nameSplit<-strsplit(as.character(awayTeam),split=" ")
nameSplit

#now grab the first name 
help(lapply)
lapply(nameSplit,head,n=1)

#get rid of list
unlist(lapply(nameSplit,head,n=1))

#or use better function for this
#return a vector or matrix if possible
sapply(nameSplit,head,n=1)

#get full city name!
getCity<-function(values){
  #if length is 3 for name split, get first two elements
  if(length(values)>2){
    return(paste(values[1],values[2],sep=" "))
  } else {#if not return first element
    return(values[1])
  }
}

#return vector of team names
sapply(nameSplit,getCity)

#may want to get the value of a function for subgroups of your data
AFinal[awayTeam=="Pittsburgh Steelers"]
mean(AFinal[awayTeam=="Pittsburgh Steelers"])

#get means of AFinal score for each team all at once
str(data)

help(tapply)
#get scores by teams
tapply(X=AFinal,INDEX=awayTeam,FUN=mean)


############################################################
##Now look at common style of output from functions

#S3 vs S4 objects

#output from a common function
boxplot(AFinal~week)
#save as object
box<-boxplot(AFinal~week)
box
#a list, just like we returned when we wrote functions!
box[[1]]
box$stats

#common function you'll use this with is lm
plot(x=AFinal,y=HFinal,main="Scatterplot of Home and Final Scores")

#fit linear model
lm(HFinal~AFinal,y=TRUE)
#save as object
fit<-lm(HFinal~AFinal,y=TRUE)
attributes(fit)

#check out what is returned
help(lm)


#Three ways to get at many of these attributes
fit[[1]]
coefficients(fit)
fit$coefficients
#residuals
fit[[2]]
residuals(fit)
fit$residuals

#no generic function for some things
rank(fit)
fit$rank

y(fit)
fit$y