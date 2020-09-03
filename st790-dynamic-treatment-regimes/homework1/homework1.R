###
# Jimmy Hickey
# 2020-08-24
# Introduction to Dynamic Treatment Regimes
# Data analysis homework 1
###
library(modelObj)
library(DynTxRegime)
library(ggplot2)

# 1

data <- read.csv(file = file.choose(), header = TRUE, sep = ",")
data$A= data$trt

y = data$chol0 - data$chol6


## i. naive
delta_N_se(y = y, A = data$A)


## ii. outcome regression
lm = buildModelObj(model = ~A + exercise + wt + smoke + trig0 + age + gender +
                     A:exercise + A:wt + A:smoke + A:trig0 + A:age + A:gender, 
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

## a
regime = as.integer({data$chol0 > 280})


### i. outcome regression
value_OR_se(moOR = lm, data = data, y = y, regime = regime, txName = 'A')


### ii. IPW

value_IPW_se(moPS = propensity, data = data,  y = y, regime = regime, txName = 'A')

### iii. alternative IPW
value_AIPW_se(moPS = propensity, moOR = lm, data = data, y = y, regime =regime, txName = 'A')



### iv. optimal AIPW
qLearn(moMain = lm, 
       moCont = lm,
       data = data, 
       response = y, 
       txName = 'A', 
       iter = 0L,
       verbose = FALSE)

## b

w = seq(110,284, length.out = 100)

regimes <- sapply(X = w, FUN = function(x,y){{y > x}*1L}, y = data$chol0)


### i. Outcome regression
lm <- buildModelObj(model = ~A+age+wt+gender+exercise+smoke+trig0+
                      A*age+A*gender+wt*gender+wt*exercise+smoke*age+chol0, 
                    solver.method = "lm", 
                    predict.method = "predict.lm", 
                    predict.args = list("type"="response"))

w = seq(110,284, length.out = 100)

regimes <- sapply(X = w, FUN = function(x,y){{y > x}*1L}, y = data$wt)




or_vals <- apply(X = regimes, 
                   MARGIN = 2L,  
                   FUN = function(x, moOR, data, y) {
                     temp <- value_OR_se(moOR = moOR, 
                                         data = data,  
                                         y = y,  
                                         regime = x, 
                                         txName = 'A')
                     return( "valueHat" = temp$valueHat)
                   },
                   moOR = peter_model,
                   data = data,
                   y = y)


### ii. IPW
ipw_vals <- apply(X = regimes, 
         MARGIN = 2L,  
         FUN = function(x, moPS, data, y) {
           temp <- value_IPW_se(moPS = moPS, 
                                data = data,  
                                 y = y,  
                                regime = x, 
                                txName = 'A')
           return( "valueHat" = temp$valueHat )
         },
         moPS = propensity,
         data = data,
         y = y)


### iii. optimal AIPW
aipw_vals <- apply(X = regimes, 
         MARGIN = 2L,  
         FUN = function(x, moPS, moOR, data, y) {
           temp <- value_AIPW_se(moPS = moPS,
                                 moOR = moOR,
                                data = data,  
                                y = y,  
                                regime = x, 
                                txName = 'A')
           return( "valueHat" = temp$valueHat )
         },
         moPS = propensity,
         moOR = peter_model,
         data = data,
         y = y)

plot_data = data.frame(or = or_vals, ipw = ipw_vals, aipw= aipw_vals, w = w)


# an unnecessarily complicated way to find the max
max_w = plot_data$w[(which.max(c(plot_data$or, plot_data$ipw,plot_data$aipw))) %% length(w)]


ggplot(data = plot_data) + 
  geom_line(aes(x = w, y = or), color = "red", linetype = "dashed") +
  geom_line(aes(x = w, y = ipw), color = "blue", linetype = "dotted") +
  geom_line(aes(x = w, y = aipw), color = "black", linetype = "solid") +
  geom_vline(xintercept = max_w)+
  ylab("V(d)") + 
  xlab("weight") + 
  scale_linetype_manual(values=c(1, 2, 3),
                          labels=c("hi", "jimmy","asdf"))
  
