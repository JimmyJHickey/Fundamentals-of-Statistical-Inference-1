###
# Jimmy Hickey
# 2020-03-19
# 
# Stochastic branching extinction
###

extinction = function(lambda, iterations)
{
 s = 0
 probabilities = c(0)
 
 for (i in 2:iterations)
 {
  probabilities[i] = exp(-lambda * (1-s))
  s = probabilities[i]
 }
 
 return(probabilities)
}


lambda = seq(1.05, 3, 0.05)

output_matrix <- matrix(ncol=3, nrow=length(lambda))

for (i in 1:length(lambda)){
 
 a = tail(extinction(lambda[i], 20), n=1)
 pi = a / lambda[i]
 
 cat("lambda:" , lambda[i])
 cat('\t')
 cat("a:" , a )
 cat('\t')
 cat("pi: ", pi)
 cat('\n')
 
 output_matrix[i,] = c(lambda[i], a, pi)
}

colnames(output_matrix) = c("lambda", "a", "pi")

plot(output_matrix[,1], output_matrix[,3],
     ylab="pi",
     xlab="lambda",
     main="Problem 1")
