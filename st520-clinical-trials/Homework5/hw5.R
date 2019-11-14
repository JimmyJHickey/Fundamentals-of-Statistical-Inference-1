###
# Jimmy Hickey
# 2019-10-31
# Find an exact confidence interval
###

###
# 1
###

# d
pchisq(33.0564, df = 3 - 1, lower.tail = FALSE)
# 6.63581e-08

# e
pnorm(0.05 / (2 * 3))
