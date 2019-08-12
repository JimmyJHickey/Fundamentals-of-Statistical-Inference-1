#############################################################################
##R script for creating samples and counting
##Justin Post 2017
#############################################################################

#Install package if needed
#load in package
library(combinat)

############################################################################
#Task 1:  Find all possible combinations of n objects taken r at a time
#This can be done using the combn function

#To get all possible combinations of the first 8 integers taken 2 at a time
combn(x=1:8,m=2)
#Each column represents a combination of 2 things from the 8 numbers 1, 2, 3, 4, 5, 6, 7, 8


#Say we had 8 elements, we will take every combination of 2 elements to be our sample space
#We can have the function return the different combinations that will make up our sample space instead of just numbers

#Let's use the committee example.  Recall: 8 members of club, 2 assigned to a committee
#Define the 8 elements. Note: quotes must be used when creating a non-numeric entry
members<-c("P1","P2","P3","P4","P5","P6","P7","P8")

#Now instead of using 1:8 as our objects, we'll use members
combn(x=members,m=2)

#All 8 choose 2 = 28 sample space elements possible!

#Let's keep this for later use
committees<-combn(x=members,m=2)

#Note: To index the elements of a matrix we use square brackets with the row index followed by the column index [i,j]
#If we want an entire row, we can leave the column blank [3,] (would give row three) and if we want an entire column, we can leave the row blank [,1] (column 1)
#First column of committees
committees[,1]

#We can also get just the number of combinations (not what they actually are) using the "nCm" function
numbercombinations<-nCm(n=8,m=2)
numbercombinations

#Another function that does this is the choose function
choose(n=8,k=2)

#notice that the variables names change depending on function used... Always consult the help file to determine this!


###################################################################################
#Task 2:  Suppose person 1 and person 2 are women and all others are men
#Now, let's find the probability that a committee will contain at least 1 woman if all are equally likely
#To compare two things in R we use the '==' command, which will return TRUE or FALSE
committees=="P1"

#Each element of the matrix is tested to see if it is equal to "P1"
p1<-committees=="P1"
p2<-committees=="P2"

#We convert these to numbers by adding the two matrices together
p1+p2

#Any column with a 1 in it contains a woman on the committee, so we can sum along the columns to get a measure of which samples have women!
#We use the apply function
help(apply)

#X is the data, MARGIN=2 implies apply the function assigned to FUN to the columns of X (1 is for the rows)
apply(X=p1+p2,FUN=sum,MARGIN=2)

#Can use the built in colSums function instead (a little faster computationally
womancommittee<-colSums(p1+p2)

#Now we can just sum the number of values that are not 0, divided by the total number of samples (!= is not equal to)
womancommittee!=0
sum(womancommittee!=0)

#Probability assuming equally likely committees:
sum(womancommittee!=0)/numbercombinations

#compare to easier method
(choose(2,1)*choose(6,1)+choose(2,2)*choose(6,0))/choose(8,2)

#####################################################################################
#Task 3: Find the previous probability using simulation
#We can randomly sample 2 of our 8 members repeatedly and see how often we get committees with women

help(sample)
#get one possible sample
sample(x=members, size=2,replace=FALSE)
#repeat process many times and save it
set.seed(10)
replicate(n=5,sample(x=members, size=2,replace=FALSE))
draws<-replicate(n=100000,sample(x=members, size=2,replace=FALSE))
#determine number of committees with women empirically
p1<-draws=="P1"
p2<-draws=="P2"
womancommittee<-colSums(p1+p2)
sum(womancommittee!=0)/100000
