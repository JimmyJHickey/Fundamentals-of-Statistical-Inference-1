###
# Jimmy Hickey
# 2019-06-20
# ST501
# Estimating real data with a distribution
#
# I am using data from of 25000 individuals heights and weights.
#
# Information about the data can be found here:
# http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_020108_HeightsWeights
# And the data itself can be found here:
# http://socr.ucla.edu/docs/resources/SOCR_Data/SOCR_Data_Dinov_020108_HeightsWeights.html
###


library(tidyverse)
library(fitdistrplus)

height_weight_data = read.csv("data/UCLA_height_weight.csv")

# Grab just the height column

# this didn't work so I just did it manually
# height <- select(height_weight_data, "Height.Inches.") %>% na.omit() %>% rename("Height" = "Height.Inches.")

height = height_weight_data[,2]


# fit a normal distribution to the data
fit <- fitdist(data = height, distr = "norm")
plot(fit)

# Check the goodness of fit of the normal distribution
gofstat(fit, fitnames = c("normal"))
# Goodness-of-fit statistics
# normal
# Kolmogorov-Smirnov statistic 0.002992866
# Cramer-von Mises statistic   0.025199932
# Anderson-Darling statistic   0.242150892
# 
# Goodness-of-fit criteria
# normal
# Akaike's Information Criterion 103086.8
# Bayesian Information Criterion 103103.0


# http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_MLB_HeightsWeights
baseball_data = read.csv("data/baseball_player_body_stats.csv")
baseball_height = baseball_data[,4]
fit_baseball <- fitdist(data = baseball_height, distr = "norm")
plot(fit_baseball)
gofstat(fit_baseball, fitnames = c("normal"))

# Goodness-of-fit statistics
# normal
# Kolmogorov-Smirnov statistic 0.09083786
# Cramer-von Mises statistic   1.57495922
# Anderson-Darling statistic   8.32988875
# 
# Goodness-of-fit criteria
# normal
# Akaike's Information Criterion 4665.045
# Bayesian Information Criterion 4674.928

##
# Now let's try using someone else's data set
# This is using the warp breaks dataset
##

# This code is by Stephanie Stewart
# She fit a Poisson ditribution

hist(warpbreaks$breaks)
fit <- fitdist(data = warpbreaks$breaks, distr = "pois")
plot(fit)

gofstat(fit, fitnames = c("pois"))


# I will try to fit a geometric distribution to the data
geom_fit <- fitdist(data = warpbreaks$breaks, distr = "geom")
plot(geom_fit)

gofstat(geom_fit, fitnames = c("geom"))

# Chi-squared statistic:  50.96223 
# Degree of freedom of the Chi-squared distribution:  6 
# Chi-squared p-value:  3.01407e-09 
# the p-value may be wrong with some theoretical counts < 5  
# Chi-squared table:
#   obscounts theocounts
# <= 15  7.000000  23.110170
# <= 18  7.000000   3.071433
# <= 21  8.000000   2.766035
# <= 26  7.000000   4.012496
# <= 29  8.000000   2.092033
# <= 36  6.000000   4.107913
# <= 44  6.000000   3.616055
# > 44   5.000000  11.223866
# 
# Goodness-of-fit criteria
# geom
# Akaike's Information Criterion 472.3441
# Bayesian Information Criterion 474.3331

nbinom_fit <- fitdist(data = warpbreaks$breaks, distr = "geom")
plot(nbinom_fit)

gofstat(nbinom_fit, fitnames = c("geom"))


