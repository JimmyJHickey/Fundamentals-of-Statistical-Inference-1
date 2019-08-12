#########################################################
##Script to simulate basic system flow example from notes
##Justin Post 2017
#########################################################

#create a function to 'test' a node
nodeWork<-function(probWork){runif(1)<=probWork}
#check if an "arm" works
nodeWork(0.9)*nodeWork(0.9)
#check if at least one arm is working
(nodeWork(0.9)*nodeWork(0.9))|(nodeWork(0.9)*nodeWork(0.9))

#repeat many times and check sample proportion of times system works
mean(replicate(10000,(nodeWork(0.9)*nodeWork(0.9))|(nodeWork(0.9)*nodeWork(0.9))))


