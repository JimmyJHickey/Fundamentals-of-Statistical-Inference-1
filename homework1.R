###
# Jimmy Hickey
# 2019-05-18
# ST501
# Homework1
# An Introduction to R
###

###
# Q1
###

# a.
# Create and save a vector (call it y) with the numbers 22,41,42,12,46,32,18,29,28.
y = c(22,41,42,12,46,32,18,29,28)
print(y)

# b. 
# Create a histogram of the y values. (Hint: use the hist function)
hist(y)

# c.
# Create a new vector (call it x) that is the y vector divided by 2.

x = y/2
print(x)

# d.
# Create a new vector (call it w) that is 1,2,3 repeated 3 times. 
# (Hint: use rep function)

w = rep(c(1,2,3), 3)
print(w)

# e.
# Create a data frame whose first column is y, second column is x, and third column
# is w (call it data).
data = data.frame(x, y, w)
print(data)

# f.
# Have R report the sum of the first row of data then use the apply 
# function to have R output the sum of each row of data.
print(sum(data[1,]))
print(rowSums(data))

# g.
# Have R report True (value greater than 20 ) or False (value less than or equal to
# 20) for each value in the data frame. Sum this matrix of T/F to find the total
# number of values that are greater than 20. Divide this by the number of values
# (check out the dim function) to find the proportion of values greater than 20.
NUM_ROW_INDEX = 1
NUM_COL_INDEX = 2
gt_num = 20

tf_matrix = (data > gt_num)
print(tf_matrix)

total_true = sum(tf_matrix)
sprintf("Number of entries greater than %d: %d", gt_num, total_true)
# Answer = 10

proportion_true = total_true / (dim(data)[NUM_ROW_INDEX] * dim(data)[NUM_COL_INDEX]) 
sprintf("Proportion of entries greater than %d: %f", gt_num, proportion_true)
# Answer = 0.370370 