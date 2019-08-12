###
# Jimmy Hickey
# 2019-07-21
# ST501
# MonteCarlo Integration Examples
###

set.seed(1978)

# a.
# Use Monte Carlo integration with n = 100 and n = 1000 to estimate
# Integrate(cos(2 * pi * x), {x,0,1})
# The actual value is 0

a_function = function(x){
  return(cos(2 * pi * x))
}

single_interal_monte_carlo = function(g_function, n, min, max, expected){
  unif_vals = runif(n, min, max)
  function_values = g_function(unif_vals)
  average = sum(function_values)/n
  error = (expected - average)/expected * 100
  cat(sprintf("The Monte Carlo estimate was: %.4f. This is %.4f %% off of the actual value of %.4f.", 
          average, error, expected))
}

# n = 100
single_interal_monte_carlo(
  g_function = a_function, 
  n = 100, 
  min = 0, 
  max = 1, 
  expected = 0)
# The Monte Carlo estimate was: -0.0273. This is -Inf % off of the actual value of 0.0000.
# (Infinity percent off because we are comparing to 0)

# n = 1000
single_interal_monte_carlo(
  g_function = a_function, 
  n = 1000, 
  min = 0, 
  max = 1, 
  expected = 0)
# The Monte Carlo estimate was: -0.0251. This is Inf % off of the actual value of 0.0000.
# (Infinity percent off because we are comparing to 0)

# b.
# Monte Carlo integration of cos[2 * pi * x^2] over (0. 1)
# The actual result from Mathematica is
# FresnelC[2]/2 = 0.244127

b_expected = 0.244127
b_function = function(x){
  return(cos(2 * pi *x^2))
}

# n = 100
single_interal_monte_carlo(
  g_function = b_function, 
  n = 100, 
  min = 0, 
  max = 1, 
  expected = b_expected)
# The Monte Carlo estimate was: 0.3627. This is -48.5664 % off of the actual value of 0.2441.

# n = 10,000
single_interal_monte_carlo(
  g_function = b_function, 
  n = 10000, 
  min = 0, 
  max = 1, 
  expected = b_expected)
# The Monte Carlo estimate was: 0.2339. This is 4.1879 % off of the actual value of 0.2441.

# n = 1,000,000
single_interal_monte_carlo(
  g_function = b_function, 
  n = 1000000, 
  min = 0, 
  max = 1, 
  expected = b_expected)
# The Monte Carlo estimate was: 0.2436. This is 0.2133 % off of the actual value of 0.2441.

# n = 100,000,000
single_interal_monte_carlo(
  g_function = b_function, 
  n = 100000000, 
  min = 0, 
  max = 1, 
  expected = b_expected)
# The Monte Carlo estimate was: 0.2442. This is -0.0410 % off of the actual value of 0.2441.