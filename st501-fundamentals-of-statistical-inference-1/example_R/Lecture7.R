####################################################
##R script corresponding to Video Lecture 7
##Lecture on writing and understanding functions
##Justin Post
###################################################

######################################################
##Looking at built in functions

#look at code for a few common functions
#correlation function
cor

#mean function
mean
#a hidden function in R
methods(mean)
#look at the default mean code
base:::mean.default

######################################################
##Writing your own function

nameOfFunction<-function(input1,input2,...){
  #code
  #return something
}

#create a basic function to standardize a vector of numeric values
#i.e. subtract off the mean and divide by the standard deviation (z-score idea)
standardize<-function(vector){
  return((vector-mean(vector))/sd(vector))
}
  
  
#use the function
data<-runif(n=10)
result<-standardize(data)
result

#check mean is 0 and sd is 1
mean(result)
sd(result)


#########################
#add more than one input
standardize<-function(vector,scale){
  if(scale==TRUE){
    return((vector-mean(vector))/sd(vector))
  } else {
    return(vector-mean(vector))
  }
}

#evaluate function  
result<-standardize(data,scale=TRUE)
result
result<-standardize(data,scale=FALSE)
result


########################
#default arguments
standardize<-function(vector,scale=TRUE){
  if(scale==TRUE){
    return((vector-mean(vector))/sd(vector))
  } else {
    return(vector-mean(vector))
  }
}

#evaluate function
result<-standardize(data,scale=TRUE)
result
#same call
result<-standardize(data)
result
    

#########################
#bringing in other arguments (already done this!)
help(plot)
methods(plot)
graphics:::plot.default

#plot that defaults to both lines and points
plotBoth<-function(x,y,...){
  plot(x,y,type="b",...)
}

#call function
plotBoth(x=1:20,y=sample(1:20,20))

#add in additional args
plotBoth(x=1:20,y=sample(1:20,20),main="Title!")


###########################
#Returning more than 1 object
standardize<-function(vector,center=TRUE,scale=TRUE){
  #get attributes to return
  mean<-mean(vector)
  stdev<-sd(vector)
  #center and scale if appropriate
  if(center==TRUE){
    vector<-vector-mean(vector)
  }
  if(scale==TRUE){
    vector<-(vector-mean(vector))/sd(vector)
  } 
  #return a list of objects
  return(list(vector,mean,stdev))
}

#evaluate function
result<-standardize(data,center=TRUE,scale=TRUE)
result
result[[2]]

###########################
#Fancy up what we return by giving names
standardize<-function(vector,center=TRUE,scale=TRUE){
  #get attributes to return
  mean<-mean(vector)
  stdev<-sd(vector)
  #center and scale if appropriate
  if(center==TRUE){
    vector<-vector-mean(vector)
  }
  if(scale==TRUE){
    vector<-(vector-mean(vector))/sd(vector)
  } 
  #return a list of objects
  return(list(data=vector,oldmean=mean,oldsd=stdev))
}

#evaluate function
result<-standardize(data,center=TRUE,scale=TRUE)
result
result$oldsd
result[[3]]



