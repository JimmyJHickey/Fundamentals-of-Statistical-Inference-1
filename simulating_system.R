###
# Jimmy Hickey
# 2019-05-28
# ST501
# Simulating System Example
###

# Create a fucntion to 'test' a node
node_work = function(prob_work){
  # Generate uniform value
  # and check if it is less than the probability of a node working
  # returns TRUE or FALSE
  runif(1)<=prob_work
}

# check if an "arm" works
node_work(0.9) * node_work(0.9)

# check if at least one arm is working
# vertical bar is a logical or
(node_work(0.9) * node_work(0.9)) | (node_work(0.9) * node_work(0.9))

# repeat many times and check sample proportion of times system works
mean(
  replicate(1000,
            (node_work(0.9) * node_work(0.9)) | (node_work(0.9) * node_work(0.9))
            )
)
