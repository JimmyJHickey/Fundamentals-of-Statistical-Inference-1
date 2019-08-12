###
# Jimmy Hickey
# 2019-06-2
# ST501
# Using the uniroot function
###

uniroot(function(x)
  {10*x^3-15*x^4+6*x^4-0.25},
  interval=c(0,1)
  )
# 0.328658