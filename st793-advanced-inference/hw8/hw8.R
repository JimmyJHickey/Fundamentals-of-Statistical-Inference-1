###
# 9.1
###

### a

sim.samp<-function(nrep,n,DIST,...){
  # simulates nrep samples from DIST of size n
  data <- matrix(DIST(n * nrep, ...), ncol = n, nrow = nrep)
}

estimators = function(data)
{
  # mean for each sample
  out.m <- apply(data,1,mean)
  # 20% trimmed mean function
  trim20<-function(x){mean(x,.2)}
  # trim20 for each sample
  out.t20 <- apply(data,1,trim20)
  # median for each sample
  out.med <- apply(data,1,median)
  
  return( list("mean" = out.m, "t20"= out.t20, "median" = out.med) )
}

library(rmutil)

set.seed(346) # sets the random number seed
mu0 = 0

n = c(10, 30, 130)
N = 100
R = 1000
n_est = 3

z_stddev = matrix(0, nrow= length(n), ncol = n_est)
colnames(z_stddev) = c("mean", "t20", "median")
rownames(z_stddev) = n

t_stddev = matrix(0, nrow= length(n), ncol = n_est)
colnames(t_stddev) = c("mean", "t20", "median")
rownames(t_stddev) = n

l_stddev = matrix(0, nrow= length(n), ncol = n_est)
colnames(l_stddev) = c("mean", "t20", "median")
rownames(l_stddev) = n


for(i in 1:length(n)){
  mse_z = matrix(0, ncol = n_est, nrow = R)
  mse_t = matrix(0, ncol = n_est, nrow = R)
  mse_l = matrix(0, ncol = n_est, nrow = R)
  
  
  for(r in 1:R){
    
    # 1000 N(0,1), t_5, laplace(0, 1) samples, sample size = n
    z <- sim.samp(N, n[i],rnorm) 
    t <- sim.samp(N, n[i], rt, 5)
    l <- sim.samp(N, n[i], rlaplace)
    
    z_est = estimators(z)
    t_est = estimators(t)
    l_est = estimators(l)
    
    mse_z[r, 1] = mean( (z_est$mean - mu0)^2 )
    mse_z[r, 2] = mean( (z_est$t20 - mu0)^2 )
    mse_z[r, 3] = mean( (z_est$median - mu0)^2 )
    
    mse_t[r, 1] = mean( (t_est$mean - mu0)^2 )
    mse_t[r, 2] = mean( (t_est$t20 - mu0)^2 )
    mse_t[r, 3] = mean( (t_est$median - mu0)^2 )
    
    mse_l[r, 1] = mean( (l_est$mean - mu0)^2 )
    mse_l[r, 2] = mean( (l_est$t20 - mu0)^2 )
    mse_l[r, 3] = mean( (l_est$median - mu0)^2 )
    
  }
  z_stddev[i, 1] = sd(mse_z[,1])
  z_stddev[i, 2] = sd(mse_z[,2])
  z_stddev[i, 3] = sd(mse_z[,3])
    
  t_stddev[i, 1] = sd(mse_t[,1])
  t_stddev[i, 2] = sd(mse_t[,2])
  t_stddev[i, 3] = sd(mse_t[,3])
  
  l_stddev[i, 1] = sd(mse_l[,1])
  l_stddev[i, 2] = sd(mse_l[,2])
  l_stddev[i, 3] = sd(mse_l[,3])
}

z_stddev
t_stddev
l_stddev

# multiply by N
N * z_stddev
N * t_stddev
N * l_stddev

