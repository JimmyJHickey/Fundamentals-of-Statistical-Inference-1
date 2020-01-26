###
# Find SVD of a matrix
# Jimmy Hickey
# 2020-01-26
###


X = as.matrix(data.frame(c(1,-1,1,-1), c(1,0,0,0), c(1,0,0,0), c(1, 1, 1, 1), c(1,1,1,1)))

# Find SVD of X
svd_X = svd(X)

# try to calculate Moore-Penrose inverse of X
# This didn't work
X_inv = svd_X$u %*% diag(svd_X$d^-1) %*% t(svd_X$v)
View(X_inv)


# Find Moore-Penrose inverse of X
library(MASS)
X_ginv = ginv(X)
X_ginv
View(X_ginv)

# Check MP inverse conditions
round(X %*% X_ginv %*% X, 3)
round(X_ginv %*% X %*% X_ginv, 3)

round(X_ginv %*% X)
round(t(X_ginv %*% X))

round(X %*% X_ginv)
round(t(X %*% X_ginv))
