###
# Jimmy Hickey
# 2019-06-23
# ST501
# Homework 5
#
# Generating random variables for specific distributions
###

# Set a seed
# 1978 is the year that Sultans of Swing was released (well re-recorder) by Dire Straits, so we'll pick that.
set.seed(1978)   

# Generate 10000 random draws between (0, 1)
n<-10000
u<-runif(n=n,min=0,max=1)

# Sanity check to make sure the u looks uniform
hist(u,main="Histogram of 10000 Random U(0,1) Values",freq=FALSE)
abline(h=1,lwd=2)

# It looks alright

### Problem 65

# set alpha
alpha_65 = 0.25

# The inverse function 
Y_65 = (-1 + sqrt(1 - 2 * alpha_65 + 4 * alpha_65 * u + alpha_65^2) )/(alpha_65)

# The CDF
Y_65_CDF = function(x, alpha){ 1/2 + x/2 + alpha/4 + (x^2 * alpha) / 4} 

# a. 
# Make a histogram of the PDF
hist(Y_65, breaks=seq(from=-1.5,to=1.5,by=0.05),main="Empirical distribution (estimate of pdf) of Y",freq=FALSE)

# Plot the CDF
plot(ecdf(Y_65),ylab="CDF estimate",xlab="y_65",main="CDF estimate for Problem 65",xlim=c(-1.25,1.25),lwd=2)

# b.
# Find the estimated probability of being between 0.8 and 0.2.
(sum(Y_65 < 0.8) - sum(Y_65 < 0.2)) / length(Y_65)
# 0.34004

# Find the actual probability of being between 0.8 and 0.2.
Y_65_CDF(0.8, alpha_65) - Y_65_CDF(0.2, alpha_65)
# 0.3375

# c.

# Find the estimated quantiles
round(quantile(Y_65), 1)
# 0%    25%    50%    75%   100% 
# -1.0  -0.4    0.1    0.6  1.0 

# Q1 = -0.4
# Median = 0.1
# Q3 = 0.6

# Find actual quantiles
c((-1 + sqrt(1 - 2 * alpha_65 + 4 * alpha_65 * 0.25 + alpha_65^2) )/(alpha_65),
  (-1 + sqrt(1 - 2 * alpha_65 + 4 * alpha_65 * 0.5 + alpha_65^2) )/(alpha_65),
  (-1 + sqrt(1 - 2 * alpha_65 + 4 * alpha_65 * 0.75 + alpha_65^2) )/(alpha_65))

# Q1 = -0.3944487
# Median = 0.1231056  
# Q3 = 0.5825757

# Problem 67

alpha_67 = 1
beta_67 = 4

# The inverse function 
Y_67 = alpha_67 * (-log(1-u))^(1/beta_67)

# The PDF
Y_67_PDF = function(x, alpha, beta){ (beta) / (alpha^beta) * x^(beta - 1) * exp(-((x/alpha)^beta)) } 

# The CDF
Y_67_CDF = function(x, alpha, beta){ 1 - exp(-(x/alpha)^beta) } 


# a. 
# Make a histogram of the PDF
hist(Y_67, main="Empirical distribution (estimate of pdf) of Y", freq=FALSE)

# Plot the CDF
plot(ecdf(Y_67),ylab="CDF estimate",xlab="y_67",main="CDF estimate for Problem 67",xlim=c(0,1.5),lwd=2)


# b.
# Find the estimated probability of being between 0.8 and 0.2.
(sum(Y_67 < 0.8) - sum(Y_67 < 0.2)) / length(Y_65)
# 0.33476

# Find the actual probability of being between 0.8 and 0.2.
Y_67_CDF(0.8, alpha_67, beta_67) - Y_67_CDF(0.2, alpha_67, beta_67)
# 0.3344855


# c.

# Find the estimated quantiles
round(quantile(Y_67), 1)

# 0%    25%  50%  75%   100% 
# 0.0   0.7  0.9  1.1   1.9 

# Q1 = 0.7
# Median = 0.9
# Q3 = 1.1

# Find the actual quantiles
c(alpha_67 * (-log(1- 0.25 ))^(1/beta_67),
  alpha_67 * (-log(1-0.5))^(1/beta_67),
  alpha_67 * (-log(1-0.75))^(1/beta_67))

# 0.7323660 0.9124443 1.0850853

# Q1 = 0.7323660
# Median = 0.9124443
# Q3 = 1.0850853