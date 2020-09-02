###
# Jimmy Hickey
# 2020-08-24
# Introduction to Dynamic Treatment Regimes
# Data analysis homework 1
###
library(modelObj)

# 1

data <- read.csv(file = file.choose(), header = TRUE, sep = ",")
data$A= data$trt

y = data$chol0 - data$chol6


## i. naive
delta_N_se(y = y, A = data$A)


## ii. outcome regression
lm <- buildModelObj(model = ~A+age+wt+gender+exercise+smoke+trig0+A*age+A*gender, 
                             solver.method = "lm", 
                             predict.method = "predict.lm", 
                             predict.args = list("type"="response"))
delta_OR_se(moOR = lm, data = data, y = y)

## iii. propensity score stratification
propensity <- modelObj::buildModelObj(model = ~ age + wt + gender + exercise + smoke + trig0 + chol0,
                              solver.method = 'glm',
                              solver.args = list(family='binomial'),
                              predict.method = 'predict.glm',
                              predict.args = list(type='response'))
delta_S_se(moPS = propensity, data = data, y = y, K = 5)


## iv. IPW
delta_IPW_se(moPS = propensity, data = data, y = y)


## v. double robust AIPW
delta_DR_se(moOR = lm, moPS = propensity, data = data, y = y)



# 2