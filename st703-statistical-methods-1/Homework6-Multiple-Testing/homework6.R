###
# Jimmy Hickey
# 2019-10-23
# ST703 Homework 6
###

# 1
qt(0.05 / (21*2), df= 96)
# -3.121027

# Scheffe
qf(0.95, df1 = 7 - 1, df2 = 96)
# 2.194516


# Tukey

# nmeans is number of groups
qtukey(0.95, nmeans = 7, df = 96)
# 4.259296