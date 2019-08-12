###
# Jimmy Hickey
# 2019-05-27
# ST501
# Homework 2
# Counting and Probability Laws
###

###
# Q1
# A closet contains n pairs of shoes
###

library(combinat)

# a.
# If 2r shoes are chosen at random (2r < n), what is the probability that there will
# be no matching pair in the sample? (For this one, no need to do coding. Simply
# put your work and answer in a commented section at the bottom of your R file.)

# ANSWER:
# Visually, you can think of it as a slot problem, where each chosen shoe is a slot
#                       ____ ____ ____ ... ____
# with restriction      2n   2n-2  2n-4 ... 2n-4r+2
# no restriction        2n   2n-1  2n-2 ... 2n-2r
# Thus, you can divide these two to get your answer.

# More rigorously,
# There are a total of C(2n, 2r) unordered ways to choose the shoes.
# There are 2^(2r) * C(n, 2r) unordered ways to choose while avoiding a pair. 
#   This can be thought of as a binary choice. Either the left or right shoe
#   can be chosen, but not both and that you are choosing 2r shoes, but can 
#   only choose from n pairs.
# Formulically: 
#             2^(2r) * C(n, 2r)
#             -----------------
#                   C(2n, 2r)

# b.
# Suppose we have 5 pairs of shoes (10 total shoes).
# Label them ‘L1, R1, L2, R2,...,L5, R5’.
# Using R, find all the possible selection of shoes
# of 2 shoes are selects, also for 4 shoes selected.

# Make vector of shoes
shoes = c("R1", "L1", "R2", "L2", "R3", "L3", "R4", "L4", "R5", "L5")

# List possible selections of 2 shoes.
combn(shoes, m=2)

# ANSWER
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
# [1,] "R1" "R1" "R1" "R1" "R1" "R1" "R1" "R1" "R1" "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2" 
# [2,] "L1" "R2" "L2" "R3" "L3" "R4" "L4" "R5" "L5" "R2"  "L2"  "R3"  "L3"  "R4"  "L4"  "R5"  "L5"  "L2"  "R3"  "L3"  "R4"  "L4"  "R5"  "L5" 
#      [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45]
# [1,] "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "R4"  "R4"  "R4"  "L4"  "L4"  "R5" 
# [2,] "R3"  "L3"  "R4"  "L4"  "R5"  "L5"  "L3"  "R4"  "L4"  "R5"  "L5"  "R4"  "L4"  "R5"  "L5"  "L4"  "R5"  "L5"  "R5"  "L5"  "L5"

# List possible selections of 4 shoes.
combn(shoes, m=4)

# ANSWER
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
# [1,] "R1" "R1" "R1" "R1" "R1" "R1" "R1" "R1" "R1" "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1" 
# [2,] "L1" "L1" "L1" "L1" "L1" "L1" "L1" "L1" "L1" "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1" 
# [3,] "R2" "R2" "R2" "R2" "R2" "R2" "R2" "L2" "L2" "L2"  "L2"  "L2"  "L2"  "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "R4"  "R4" 
# [4,] "L2" "R3" "L3" "R4" "L4" "R5" "L5" "R3" "L3" "R4"  "L4"  "R5"  "L5"  "L3"  "R4"  "L4"  "R5"  "L5"  "R4"  "L4"  "R5"  "L5"  "L4"  "R5" 
#       [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46]
# [1,] "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1" 
# [2,] "L1"  "L1"  "L1"  "L1"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2" 
# [3,] "R4"  "L4"  "L4"  "R5"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "R4"  "R4"  "R4" 
# [4,] "L5"  "R5"  "L5"  "L5"  "R3"  "L3"  "R4"  "L4"  "R5"  "L5"  "L3"  "R4"  "L4"  "R5"  "L5"  "R4"  "L4"  "R5"  "L5"  "L4"  "R5"  "L5" 
#      [,47] [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62] [,63] [,64] [,65] [,66] [,67] [,68]
# [1,] "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1" 
# [2,] "R2"  "R2"  "R2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2"  "R3"  "R3"  "R3"  "R3" 
# [3,] "L4"  "L4"  "R5"  "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "R4"  "R4"  "R4"  "L4"  "L4"  "R5"  "L3"  "L3"  "L3"  "L3" 
# [4,] "R5"  "L5"  "L5"  "L3"  "R4"  "L4"  "R5"  "L5"  "R4"  "L4"  "R5"  "L5"  "L4"  "R5"  "L5"  "R5"  "L5"  "L5"  "R4"  "L4"  "R5"  "L5" 
#      [,69] [,70] [,71] [,72] [,73] [,74] [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86] [,87] [,88] [,89] [,90]
# [1,] "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "R1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1" 
# [2,] "R3"  "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "L3"  "L3"  "R4"  "R4"  "R4"  "L4"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2" 
# [3,] "R4"  "R4"  "R4"  "L4"  "L4"  "R5"  "R4"  "R4"  "R4"  "L4"  "L4"  "R5"  "L4"  "L4"  "R5"  "R5"  "L2"  "L2"  "L2"  "L2"  "L2"  "L2" 
# [4,] "L4"  "R5"  "L5"  "R5"  "L5"  "L5"  "L4"  "R5"  "L5"  "R5"  "L5"  "L5"  "R5"  "L5"  "L5"  "L5"  "R3"  "L3"  "R4"  "L4"  "R5"  "L5" 
#      [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98] [,99] [,100] [,101] [,102] [,103] [,104] [,105] [,106] [,107] [,108] [,109] [,110]
# [1,] "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"  "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"  
# [2,] "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"  "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "L2"   "L2"   "L2"   "L2"   "L2"  
# [3,] "R3"  "R3"  "R3"  "R3"  "R3"  "L3"  "L3"  "L3"  "L3"  "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "R3"   "R3"   "R3"   "R3"   "R3"  
# [4,] "L3"  "R4"  "L4"  "R5"  "L5"  "R4"  "L4"  "R5"  "L5"  "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "L3"   "R4"   "L4"   "R5"   "L5"  
#      [,111] [,112] [,113] [,114] [,115] [,116] [,117] [,118] [,119] [,120] [,121] [,122] [,123] [,124] [,125] [,126] [,127] [,128] [,129]
# [1,] "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"  
# [2,] "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"  
# [3,] "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L4"  
# [4,] "R4"   "L4"   "R5"   "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "R4"   "L4"   "R5"   "L5"   "L4"   "R5"   "L5"   "R5"   "L5"  
#      [,130] [,131] [,132] [,133] [,134] [,135] [,136] [,137] [,138] [,139] [,140] [,141] [,142] [,143] [,144] [,145] [,146] [,147] [,148]
# [1,] "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "L1"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"  
# [2,] "R3"   "L3"   "L3"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"  
# [3,] "R5"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "L4"   "L4"   "R5"   "R5"   "R3"   "R3"   "R3"   "R3"   "R3"   "L3"   "L3"   "L3"  
# [4,] "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "R5"   "L5"   "L5"   "L5"   "L3"   "R4"   "L4"   "R5"   "L5"   "R4"   "L4"   "R5"  
#      [,149] [,150] [,151] [,152] [,153] [,154] [,155] [,156] [,157] [,158] [,159] [,160] [,161] [,162] [,163] [,164] [,165] [,166] [,167]
# [1,] "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"  
# [2,] "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "L3"   "L3"  
# [3,] "L3"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "R4"   "R4"  
# [4,] "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "R4"   "L4"   "R5"   "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "L4"   "R5"  
#      [,168] [,169] [,170] [,171] [,172] [,173] [,174] [,175] [,176] [,177] [,178] [,179] [,180] [,181] [,182] [,183] [,184] [,185] [,186]
# [1,] "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "R2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"  
# [2,] "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "L3"  
# [3,] "R4"   "L4"   "L4"   "R5"   "L4"   "L4"   "R5"   "R5"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "R4"  
# [4,] "L5"   "R5"   "L5"   "L5"   "R5"   "L5"   "L5"   "L5"   "R4"   "L4"   "R5"   "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "L4"  
#      [,187] [,188] [,189] [,190] [,191] [,192] [,193] [,194] [,195] [,196] [,197] [,198] [,199] [,200] [,201] [,202] [,203] [,204] [,205]
# [1,] "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "L2"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"   "R3"  
# [2,] "L3"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"   "L3"   "L3"   "L3"   "L3"   "L3"   "L3"   "R4"   "R4"   "R4"   "L4"  
# [3,] "R4"   "R4"   "L4"   "L4"   "R5"   "L4"   "L4"   "R5"   "R5"   "R4"   "R4"   "R4"   "L4"   "L4"   "R5"   "L4"   "L4"   "R5"   "R5"  
# [4,] "R5"   "L5"   "R5"   "L5"   "L5"   "R5"   "L5"   "L5"   "L5"   "L4"   "R5"   "L5"   "R5"   "L5"   "L5"   "R5"   "L5"   "L5"   "L5"  
#      [,206] [,207] [,208] [,209] [,210]
# [1,] "L3"   "L3"   "L3"   "L3"   "R4"  
# [2,] "R4"   "R4"   "R4"   "L4"   "L4"  
# [3,] "L4"   "L4"   "R5"   "R5"   "R5"  
# [4,] "R5"   "L5"   "L5"   "L5"   "L5" 

# c.
# For each case, use R to find the probability that
# you do not select a pair (using the enumerated possibilities
# not the formula from (a)). Check that each of these
# matches the probability you found in part (a).


# Function: check_no_shoe_pairs
# A function to check if there are no pairs of shoes
# input: shoe_vector - A vector of shoes. Each entry
#   should be a string of the form "FOOTNUMBER" 
# e.g. "L1", "R4"
# return: TRUE if there are no pairs (two shoes of the same number) in the array
check_no_shoe_pairs = function(shoe_vector){
  SHOE_NUMBER_LOCATION = 2
  found_shoes = c()
  no_pair = TRUE
  
  # iterate over input shoes
  for (i in shoe_vector){
    shoe_number = substr(i, SHOE_NUMBER_LOCATION, SHOE_NUMBER_LOCATION)
    
    # if a shoe's pair has been found, set no_pair to FALSE and break
    # otherwise, add the current shoe to the list of found shoes
    if (shoe_number %in% found_shoes){
      no_pair = FALSE
      break
    }
    else{
       found_shoes = append(found_shoes, shoe_number)
    }
  }
  return(no_pair)
}


# Function: no_pair_probability
# Find the probability that no two shoes from a pair are chosen.
# input: number_of_pairs - Number of pairs of shoes.
# input: number_chosen - Number of shoes chosen in each sample.
# return: no_pair_prob - The probability that there are no pairs chosen.
no_pair_probability = function(number_of_pairs, number_chosen){
  
  # Create an array with all the shoes for all the pairs
  pairs_seq = seq(1, number_of_pairs)
  shoes = c()
  
  for (i in pairs_seq){
    shoes = append(shoes, paste("R", i, sep=""))
    shoes = append(shoes, paste("L", i, sep=""))
  }
  
  # Find all combinations when choosing number_chosen shoes
  choose_n_shoes = combn(shoes, m = number_chosen)
  
  # Array of all columns with no pairs
  no_pair_arr = apply(X = choose_n_shoes, FUN = check_no_shoe_pairs, MARGIN = 2)

  # Probability that there are no pairs
  prob_no_pair = sum(no_pair_arr) / (dim(choose_n_shoes)[2])
  return(prob_no_pair)
}

no_pair_probability(number_of_pairs = 5, number_chosen = 2)
# ANSWER
# 0.8888889

no_pair_probability(number_of_pairs = 5, number_chosen = 4)
# ANSWER
# 0.3809524

