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

# log likelihood
llik = function(mu, sigma, dta=data.max){
  n = length(dta)
  return(
    -n * log(sigma) + (-sum(dta) + n * mu)/sigma - sum(exp( (-dta + mu)/sigma ))
  )
}


mu <- seq(1000,5000, length.out =1000)
sigma  <- seq(1000,5000, length.out =1000)
mu_grid <- rep(mu, each=1000)
sigma_grid  <- rep(sigma, 1000)
out <- mapply( FUN=llik , mu_grid , sigma_grid)

out_mat <- matrix(out, nrow=1000, byrow = TRUE)

require(fields)

image.plot(mu, sigma, out_mat)


# NLM minimizes so take the negative to find the max
nllik = function(theta,dta=data.max){
  return(-llik(mu=theta[1], sigma=theta[2], dta=dta))
}


nlm_out <- nlm(nllik, c(1,2), dta=data.max)
nlm_out
# $minimum
# [1] 319.3639
# 
# $estimate
# [1] 4395.147 1882.499
# 
# $gradient
# [1]  2.904802e-08 -2.645145e-08
# 
# $code
# [1] 1
# 
# $iterations
# [1] 36




## b

muhat=nlm_out$estimate[1]
sigmahat=nlm_out$estimate[2]

par(mfrow = c(2,1)) # gives two plots per page
qextval<-function(t,mu,sigma){-sigma*log(-log(t))+mu}
pextval<-function(x,mu,sigma){exp(-exp(-(x-mu)/ sigma))}

plot(qextval(ppoints(data.max),0,1),sort(data.max))
seq(1900,13200,,100)->x # a grid of values
pextval(x,muhat,sigmahat)->y # est. cdf for grid
plot(x,y,type="l") # plots est. ext. value cdf
1:35/35->ht # heights for empirical cdf
points(sort(data.max),ht) # adds empirical cdf

# QQ Plot looks like a straight line (which means we have a good estimate) and the CDF follows the data fairly well.

