###
# Jimmy Hickey
# 2019-07-1
# ST501
# Plotting practice for homework 6
###


# 3.8 a. i.

plot(1, type="n", xlab="X", ylab="Y", xlim=c(-0.25, 1.25), ylim=c(-0.25, 1.25), main = "3.8 a. i.")
x_1 = abline(v = 1, col = "#0000ff")
y_1 = abline(h = 1, col = "#0000ff")
x_0 = abline(v = 0, col = "#0000ff")
y_0 = abline(h = 0, col = "#0000ff")

polygon(x = c(0 , 1, 1),
        y = c(0, 1, 0),
        col = "#ff0000")


# 3.8 a. ii.

plot(1, type="n", xlab="X", ylab="Y", xlim=c(-0.25, 1.25), ylim=c(-0.25, 1.25), main = "3.8 a. ii.")
x_1 = abline(v = 1, col = "#0000ff")
y_1 = abline(h = 1, col = "#0000ff")
x_0 = abline(v = 0, col = "#0000ff")
y_0 = abline(h = 0, col = "#0000ff")

polygon(x = c(0 , 0, 1),
        y = c(0, 1, 0),
        col = "#ff0000")

# 3.8 a. iii. 

plot(1, type="n", xlab="X", ylab="Y", xlim=c(-0.25, 1.25), ylim=c(-0.25, 1.25), main = "3.8 a. iii.")
x_1 = abline(v = 1, col = "#0000ff")
y_1 = abline(h = 1, col = "#0000ff")
x_0 = abline(v = 0, col = "#0000ff")
y_0 = abline(h = 0, col = "#0000ff")

polygon(x = c(0 , 0, 1/2, 1/2),
        y = c(0, 1, 1, 0),
        col = "#ff0000")


# 3.18 a. i.

plot(1, type="n", xlab="X", ylab="Y", xlim=c(-0.25, 1.25), ylim=c(-0.25, 1.25), main = "3.18 a.")
x_1 = abline(v = 1, col = "#0000ff")
x_0 = abline(v = 0, col = "#0000ff")
y_0 = abline(h = 0, col = "#0000ff")

polygon(x = c(0 , 1, 1),
        y = c(0, 1, 0),
        col = "#ff0000")

