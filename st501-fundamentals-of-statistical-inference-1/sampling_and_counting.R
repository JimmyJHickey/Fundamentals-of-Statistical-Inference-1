###
# Jimmy Hickey
# 2019-05-22
# ST501
# Sampling and Counting
###

##
# Simulation is a very important idea
# 
# Use a computer to create data that corresponds to a situation
# Perform whatever calculations necessary for that data
# Repeate the process many times
# Use a sample proportion as an approximate probability

library("combinat")

##
# Task 1
# Find all possible combinations of n objects taken 
# r at a time
# This can be done with the combn function

# To get all possible comibnations of the first 8 
# integrs taken 2 at a time:
# Each column represents a combination of 2 things
# from the 8 numbers 1, 2, 3, 4, 5, 6, 7, 8
combn(x = 1:8, m =2)


# Say we had 8 elements, we wil take every combination of 2 elements
# to  be our sample space.
# We can have the function return the different combinations that 
# will make up our sample space instead of just numbers/

# Let's use the committee example. 
# Recall: 8 members of club, 2 assigned to a committe
# Define 8 elements
members = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")
combn(members, m = 2)

committees = combn(members, m = 2)

# Note: To index the elements of a matric we use square brackets with
# row index followed by column index [i, j]
# If we want to grab an entire row, we can leave the column blank.
# e.g. [3,] grabs all of row 3
# If we want to grab an entire column, we can leave the row blank.
# e.g. [,4] grabs all of column 4

committees[2,20]
committees[,1]
committees[2,]

# We can also just get the number of combinations 
# (not what they actually are) by using the "nCm" function
number_of_combinations = nCm(n = 8, m = 2)
number_of_combinations

# Or use the "choose" function
choose(n = 8, k = 2)

##
# Task 2
# Suppose person 1 and person 2 are women and all other are men
# Now, let's find the probability that a committee will contain
# at least one woman if all are equally likely
# To compare two things in R we use the '==' command which will
# return TRUE or FALSE

committees == "P1"

# Each element of the matrix is tested to see if it is equal to "P1"
p1 = committees == "P1"
p2 = committees == "P2"

# We convert these to numbers by adding the two matrices together
p1 + p2

# Any column with a 1 in it contains a woman on the committee
# So we can sum along the columns
# We use the apply function
help(apply)

# X is the data
# MARGIN = 2 implies apply the function 
#   assigned to FUN to the columns of X (1 for rows)
apply(X = p1+p2, FUN = sum, MARGIN = 2)

# Can use the build in colSums function instead
# It is a little faster computationally
woman_committee = colSums(p1+p2)

# Now we can just sum the number of values that are not 0,
# divided by the total number of samples
woman_committee != 0
sum(woman_committee != 0)

# Probability assuming equally likely committess:
sum(woman_committee != 0) / number_of_combinations

# Compare to easier method
( (choose(2,1) * choose(6,1)) + (choose(2,2) *choose(6,0)) ) / (choose(8,2))

##
# Task 3
# Find the previous probability using simulation
# We can randomly sample 2 of our 8 members repeatedly
# and see how often we get committees with women

help(sample)

# get one sample
sample(x = members, size = 2, replace = FALSE)

# repeat process many times and save it
set.seed(10)

replicate(
  n = 5, 
  sample(x = members, size = 2, replace = FALSE)
)

draws = replicate(
  n=100000,
  sample(x = members, size = 2, replace = FALSE)
  )

p1 = draws=="P1"
p2 = draws == "P2"

woman_committee = colSums(p1+p2)
sum(woman_committee != 0) / 100000

