###
# Jimmy Hickey
# 2019-11-04
# ST703 Homework 7
###

###
# 2
###


# d

pt(0.10 / 2, df = 20 - 5)
# 0.5196089


# g

qt(0.05/2, df = 20-2)


# h

qt(0.05, df = 20 - 5 - 1)
# 1.76131

###
# 3
###

qf(0.95, df1 = 2, df2 = 16-4)
# 3.885294




###
# 4
###

# e i
qf( 0.95, df1 = 4, df2 = 45)


# e ii

# nmeans is number of groups
qtukey(0.95, nmeans = 5, df = 50 - 5)
# 4.018417

4.018417 * sqrt(1/2)
# 2.84145