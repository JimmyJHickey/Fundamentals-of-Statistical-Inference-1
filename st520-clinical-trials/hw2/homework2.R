###
# Jimmy Hickey
# 2019-09-08
# Find an exact confidence interval
###

# n = 20
# k = 5
# half_CI = (1-0.80)/2
# 
# find_upper_CI = function(p)
# {
#   pbinom(k, n, p) - 0.1 
# }
# 
# find_lower_CI = function(p)
# {
#   pbinom(k-1, n, p) - (1 - 0.1)
# }
# 
# piU = uniroot(find_upper_CI, c(0,1))$root
# piL = uniroot(find_lower_CI, c(0,1))$root
# 
# c(piL, piU)

library(Hmisc)
binconf(k,n,alpha=.10,method="all")
# CI: (0.10408084, 0.4555824)

# c.

ci_proportion = function(p)
{
  prob = 0
  for(i in 0:20)
  {
    bounds = binconf(i, 20, alpha=0.10, method="exact")

    if( bounds[2] <= p && p <= bounds[3] )
    {
      prob = prob + dbinom(i, 20, p)
    }
  }
  return(prob)
}

ci_proportion(0.2)
# 0.9563281

ci_proportion(0.5)
# 0.9586105


###
# 3
###

binom_sum = function(n, k, p)
{
  total = 0
  for (i in k:n )
  {
    total = total + choose(n, i) * p^i * (1-p)^(n-i)
  }
  return(total)
}

p_success_stage_1 = function(p)
{
  return(p^3)
}

p_success_stage_2 = function(p)
{
  return(choose(3, 1) * p * (1-p)^2 * sum(dbinom(4:5, 5, p)) + choose(3,2) * p^2 * (1-p) * sum(dbinom(3:5, 5, p)))
}

p_success = function(p)
{
  return(p_success_stage_1(p) + p_success_stage_2(p))
}

# i.
p_success(p = 0.25)
# 0.03677368

# ii.
p_success( p = 0.50)
# 0.3828125

# iii.
p_success(p = 0.75)
# 0.8890686

# b.

p_stopping

expected_sample_size = function(n, n0, p)
{
  p_stopping_stage_1 = p^3 + (1-p)^3
  return(n0 * p_stopping_stage_1 + n * (1 - p_stopping_stage_1 ))
}

# i.
expected_sample_size(n=8 , n0=3 , p=0.25)
# 5.8125

# ii.
expected_sample_size(n=8 , n0=3 , p=0.5)
# 6.75

# iii.
expected_sample_size(n=8, n0=3, p=0.75)
# 5.8125







### DANIELLE SAVES THE DAY

prob_success = function(p)
{
  return(dbinom(3,3, pi) + dbinom(1,3, pi) * (1 - pbinom(3,5,pi)) + dbinom(2,3,pi) * (1-pbinom(2,5, pi))
)
}

# 3.

# a.
# i.
prob_success(0.25)  
# 0.03677368

# ii.
dbinom(3,3, 0.5) + dbinom(1,3, 0.5) * (1 - pbinom(3,5,0.5)) + dbinom(2,3, 0.5) * (1-pbinom(2,5, 0.5))
# 0.3828125

# iii.
dbinom(3,3, 0.75) + dbinom(1,3, 0.75) * (1 - pbinom(3,5,0.75)) + dbinom(2,3, 0.75) * (1-pbinom(2,5, 0.75))
# 0.8890686

# b.
# i
calc_n_people = function(p)
{
  return(3 * (dbinom(0,3, p) + dbinom(3, 3, p)) + 
           8 * (1 - (dbinom(0,3,p) + dbinom(3, 3,p))))
}



pi = 0.25

3 * (dbinom(0,3, 0.25) + dbinom(3, 3, 0.25)) + 8 * (1 - (dbinom(0,3,0.25 ) + dbinom(3, 3, 0.25)))

pi =0.75
k = dbinom(0,3,pi) + dbinom(3,3,pi)
3 * k + 8 * (1-k)
 
