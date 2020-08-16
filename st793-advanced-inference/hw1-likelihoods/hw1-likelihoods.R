###
# Jimmy Hickey
# 2020-08-16
# Hw1: Likelihoods
###


###
# 2.2
###

data.max=c(
  5550, 4380, 2370, 3220, 8050, 4560, 2100,
  6840, 5640, 3500, 1940, 7060, 7500, 5370,
  13100, 4920, 6500, 4790, 6050, 4560, 3210,
  6450, 5870, 2900, 5490, 3490, 9030, 3100,
  4600, 3410, 3690, 6420, 10300, 7240, 9130
)

## a



## b

par(mfrow = c(2,1)) # gives two plots per page
qextval<-function(t,mu,sigma){-sigma*log(-log(t))
  +mu}
pextval<-function(x,mu,sigma){exp(-exp(-(x-mu)/
                                         sigma))}
plot(qextval(ppoints(data.max),0,1),sort(data.max))
seq(1900,13200,,100)->x # a grid of values
pextval(x,muhat,sigmahat)->y # est. cdf for grid
plot(x,y,type="l") # plots est. ext. value cdf
1:35/35->ht # heights for empirical cdf
points(sort(data.max),ht) # adds empirical cdf

