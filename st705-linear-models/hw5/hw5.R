library(MASS)
library(pracma)

X = matrix(
  c(
    1, 1, 0, 1, 0, 0,
    1, 1, 0, 0, 1, 0,
    1, 1, 0, 0, 0, 1,
    1, 0, 1, 1, 0, 0,
    1, 0, 1, 0, 1, 0,
    1, 0, 1, 0, 0, 1
  ),
  nrow = 6,
  ncol = 6,
  byrow = TRUE
)

# MASS
Null(t(X))

# pracma
null(X)

