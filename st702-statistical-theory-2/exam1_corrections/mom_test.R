###
#
# Jimmy Hickey
# 2020-02-25
# 
# Testing two method of moment calculations
# 
###

mom_jimmy = function(x)
{
  n = length(x)
  m1 = 1 / n * sum(x)
  m2 = 1/ n * sum(x^2)
  
  alpha_hat = (m1)/( (m2/m1) - m1 )
  beta_hat = m2/m1 - m1
  
  return(c(alpha_hat,beta_hat))
}


mom_martin = function(x)
{
 n = length(x)
 xbar = 1 / n * sum(x)
 x_minus_xbar_sq = sum((x-xbar)^2)
 
 alpha_hat = xbar^2 / (1/n * x_minus_xbar_sq)
 beta_hat = (1/n * x_minus_xbar_sq) / xbar
 
 return(c(alpha_hat, beta_hat))
}


test_times = function(nruns, nsamples)
{
  
  jimmy_sum = 0
  martin_sum = 0
  
  for (i in 1:nruns)
  {
    x = rnorm(nsamples, mean = 0, sd = 1)
    
    jimmy_sum = jimmy_sum +  system.time(
      mom_jimmy(x)
    )[3]
    
    martin_sum = martin_sum + system.time(
      mom_martin(x)
    )[3]
  }
  
  return(c(jimmy_sum/nruns, martin_sum/nruns))
}

test_times(100, 10000000)
