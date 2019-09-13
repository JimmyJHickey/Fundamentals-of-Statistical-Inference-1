###
# Jimmy Hickey
# 2019-09-08
# Find an exact confidence interval
###

n = 20
k = 5
half_CI = (1-0.80)/2

find_upper_CI = function(p)
{
  pbinom(k, n, p) - 0.1 
}

find_lower_CI = function(p)
{
  pbinom(k-1, n, p) - (1 - 0.1)
}

piU = uniroot(find_upper_CI, c(0,1))$root
piL = uniroot(find_lower_CI, c(0,1))$root

c(piL, piU)

library(Hmisc)
binconf(k,n,alpha=.10,method="all")
# CI: (0.10408084, 0.4555824)

# c.
pbinom(5, 20, 0.2)
# 0.8042078

pbinom(5, 20, 0.5)
# 