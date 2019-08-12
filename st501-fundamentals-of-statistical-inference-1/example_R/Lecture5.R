####################################################
##R script corresponding to Video Lecture 5
##Lecture on logical comparisons, if/then/if else, and some string manipulation
##Justin Post
###################################################

######################################################
##logical comparisons

#Use of ==, !=
"hi"=="hello"
"hi"=="hi"
4==1
4>=3
4!=1
"hi"!="hello"

#comparison similar to is. functions we looked at before
is.numeric("Word")
is.numeric(10)
is.numeric("10")

#toy example
a<-1:30
b<-sample(x=1:30,size=30,replace=TRUE)
b
a==b
a[1]==b


###useful for indexing a vector
data("iris")
head(iris)
attach(iris)
Species=="setosa"

#math on TRUE and FALSE
(Species=="setosa")+0
index<-Species=="setosa"
Sepal.Length[1:50]
Sepal.Length[index]

#which function can be useful
index1<-which(Species=="setosa")
#same result
Sepal.Length[index]
Sepal.Length[index1]

#see how many "setosa" there are
sum(index1)
#same as 
table(Species)
table(Species)[1]

index2<-Species!="setosa"
index2

######################################################
#If then, If then else
#if then statement
if(condition){
  execute this code
}

#if then else
if (condition){
  execute this code  
} else {
  execute this code
}

#or more if statements
if (condition){
  execute this code  
} else if (condition2) {
  execute this code
} else if (condition3){
  execute this code
} else {
  execute this code
}

#quick example
a<-5
if (a<10){
  print("hi")
}

if (a<10){
  print("hi")
} else {
  print("goodbye")
}

if (a<10){
  print("hi")
} else if (a<40){
  print("goodbye")
} else {
  print("what")
}

####### &&, & and ||, |
x<-runif(n=20,min=0,max=2)
x
x>0.5
x<1.5
#and
(x>0.5)&(x<1.5)
(x>0.5)&&(x<1.5)

index2<-(Species!="setosa")&(Species!="virginica")

#ors
(x<0.5)|(x>1.5)
(x<0.5)||(x>1.5)

if((x[1]<0.5)|(x[1]>1.5)){
  y<-0
} else {
  y<-1
}
y

######ifelse function (if else on a vector)
#see which elements are equal, do one of two things depending on result
ifelse((x<0.5)|(x>1.5),x-10,x)
ifelse((x>0.5)&(x<1.5),"Cow","Horse")


#########################################
##basic string manipulations and paste
colnames(iris)

##strsplit
help(strsplit)
strsplit(x=colnames(iris),split="[.]")
names<-strsplit(x=colnames(iris),split="[.]")
#returns a list!
names[[1]]

#turn a basic list into a vector
unlist(strsplit(x=colnames(iris),split="[.]"))

##paste function
paste
paste("Hi","What","Is","Going","On","?",sep=" ")
paste("Hi","What","Is","Going","On","?",sep=".")

#Useful when making plot titles
index3<-(Species=="virginica")
#Species is a factor, get the levels
levels(Species)
#make 3 plots
par(mfrow=c(1,3))

plot(Petal.Length[index1],main=paste("Petal Length of ",levels(Species)[1]))
plot(Petal.Length[index2],main=paste("Petal Length of ",levels(Species)[2]))
plot(Petal.Length[index3],main=paste("Petal Length of ",levels(Species)[3]))

paste("Petal Length of ",levels(Species))
