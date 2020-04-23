###
#
# Jimmy Hickey
# 2020-04-14
# Midterm 2 Notes
#
# I may need providential help for this one.
#
###



# 3

## c

x = seq(0, 3, 0.1)
th1 = 2
th2 = 3

fx = th2 / th1 * x^(th2 - th1) * exp( x^(th1) - x^(th2) )

plot(x, fx)
