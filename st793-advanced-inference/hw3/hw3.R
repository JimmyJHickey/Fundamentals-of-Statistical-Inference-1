###
# Jimmy Hickey
# 2020-09-03
# ST 793
# Homework 3 on information matrices
###


# 2.42
set.seed(1978)

mu = 1
sigma = 1
n = 100

ndata = exp(rnorm(n, mu, sigma))


nllik = function(theta,dta=ndata){
  mu = theta[1]
  sigma = theta[2]
  lambda = theta[3]
  
  # force positive lambda
  psi = log(lambda)
  
  n = length(dta)

  
  if(lambda == 0)
  {
    like = - n/2 * log(2 * pi * sigma^2) + -1 / (2 *sigma^2)*(  sum( (log(dta) - mu)^2  ))
  }
  else if(lambda != 0)
  {
    like = (psi - 1) * sum(log(dta)) - n/2 * log(2 * pi * sigma^2) +
      -1 / (2 *sigma^2)*sum( ((dta^psi - 1)/psi - mu)^2)
  }
  
  return(-like) 
  
}

nllik(c(1,2,3), ndata)
nllik(c(1,2,0), ndata)

n_iter = 1000
estimates = data.frame(mu=double(n_iter),
                       sigma=double(n_iter),
                       lambda=double(n_iter))


for (i in 1:n_iter){
  ndata = exp(rnorm(n, mu, sigma))
  nlm_out <- nlm(nllik, c(2,2,2), dta=ndata)
  estimates$mu[i] = nlm_out$estimate[1]
  estimates$sigma[i] = nlm_out$estimate[2]
  
  # transform back from psi
  estimates$lambda[i] = exp(nlm_out$estimate[3])
}


Ihat = cov(estimates)
Ihat

mu = 1
sigma = 1 
lambda = 0

eta1 = (7 * sigma^2 + 2 * mu^2 + mu^4/sigma^2) / 6
eta2 = (1  + mu^2 / sigma^2)

iinv_11 = eta1
iinv_12 = mu * sigma * eta2 / 3
iinv_13 = eta1 / 3
iinv_21 = mu * sigma * eta2 / 3
iinv_22 = sigma^2/2 + (2 * mu^2)/3 
iinv_23 = (2 * mu) / (3 * sigma)
iinv_31 = eta1 / 3
iinv_32 = (2 * mu) / (3 * sigma)
iinv_33 = 2 / (3 * sigma^2)

I = matrix( c(iinv_11, iinv_12, iinv_13,
               iinv_21, iinv_22, iinv_23, 
               iinv_31, iinv_32, iinv_33), nrow = 3, byrow = TRUE)
I

relative_f_dist = norm(Ihat-I, type="F")/norm(I, type="F")
relative_f_dist
