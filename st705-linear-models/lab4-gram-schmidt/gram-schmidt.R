# Gram-Schmidt process for constructing orthogonal set of vectors 
# from a set of linearly independent vectors.
gramschmidt = function(X)
{
  num_col = ncol(X)
  num_row = nrow(X)
  
  U = matrix(nrow = num_row, ncol = num_col)
  U[,1] = X[,1]
  
  null_proj = diag(num_row)
  
  
  for (i in 2:num_col)
  {
    U_prev = U[, i-1, drop = FALSE]
    
    null_proj = null_proj - (U_prev %*% t(U_prev) / sum(U_prev^2))
    
    U[,i] = null_proj %*% X[,i]
  }
  return(U)
}

X = matrix(
  c(1, 1, 0,
  1, 0, 1,
  1, 1, 1,
  1, 1, 0,
  1, 0, 1),
  nrow = 5, ncol = 3, byrow = TRUE)


U = gramschmidt(X)
U