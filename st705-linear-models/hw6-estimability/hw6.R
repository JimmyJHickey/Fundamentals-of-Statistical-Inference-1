library(MASS)

X = matrix(
  c(
    1, 1, 0, 1, 
    1, 1, 0, 2, 
    1, 1, 0, 3, 
    1, 1, 0, 4, 
    1, 0, 1, 1, 
    1, 0, 1, 2, 
    1, 0, 1, 3, 
    1, 0, 1, 4
  ),
  nrow = 8,
  ncol = 4,
  byrow = TRUE
)

ginv( t(X) %*% X )
