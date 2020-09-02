###
# Code from # http://www.laber-labs.com/dtr-book/Chapter2/treatment_effect.html
###

#------------------------------------------------------------------------------#
# naive estimator of tx effect
#
# ASSUMPTIONS
#   tx is binary coded as 0,1
#
# INPUTS
#  y : a vector containing the outcome of interest
#  A : a vector containing the tx received
#
# RETURNS
#  a list containing
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the naive estimator for the tx effect
#------------------------------------------------------------------------------#
delta_N <- function(y, A) {
  
  #### Average Responses
  
  # aggregate data for mean in each tx
  EY <- stats::aggregate(x = y, by = list(A), FUN = mean)
  
  # convert to named vector
  EY <- array(data = EY[,2L], dimnames = list(EY[,1L]))
  
  #### Treatment Effect
  
  delta <- unname(obj = EY[2L] - EY[1L])
  
  return( list("EY" = EY, "deltaHat" = delta) )
}

#------------------------------------------------------------------------------#
# naive estimator of tx effect and its standard error
#
# REQUIRES
#   delta_N()
#
# ASSUMPTIONS
#   tx is binary coded as {0,1}
#
# INPUTS
#  y : a vector containing the outcome of interest
#  A : a vector containing the tx received
#
# RETURNS
#  a list containing
#  deltaHat : the naive estimator for the tx effect
#        EY : the sample mean of the outcome under each tx option
#  sigmaHat : the estimated standard error
#------------------------------------------------------------------------------#
delta_N_se <- function(y, A) {
  
  # tx effect
  delta <- delta_N(y = y, A = A)
  
  # estimated tx effect each individual
  delta_i <- y * A / mean(x = A) - y * {1.0 - A} / mean(x = 1.0 - A)
  
  # variance
  sigmaSq <- mean(x = {delta_i - delta$deltaHat}^2L)
  
  return( c(delta, "sigmaHat" = sqrt(x = sigmaSq / length(x = y))) ) 
  
}


#------------------------------------------------------------------------------#
# components of the sandwich estimator for an ordinary least squares estimation
#   of a linear regression model
#
# ASSUMPTIONS
#   mo is a linear model with parameters to be estimated using OLS
#
# INPUTS
#    mo : a modeling object specifying a regression step
#         *** must be a linear model ***
#  data : a data.frame containing covariates and tx
#     y : a vector containing the outcome of interest
#
#
# RETURNS
# a list containing
#     MM : M^T M component from OLS
#  invdM : inverse of the matrix of derivatives of M wrt beta
#------------------------------------------------------------------------------#
swv_OLS <- function(mo, data, y) {
  
  n <- nrow(x = data)
  
  fit <- modelObj::fit(object = mo, data = data, response = y)
  
  # Q(X,A;betaHat)
  Qa <- drop(x = modelObj::predict(object = fit, newdata = data))
  
  # derivative of Q(X,A;betaHat)
  dQa <- stats::model.matrix(object = modelObj::model(object = mo), 
                             data = data)
  
  # number of parameters in model
  p <- ncol(x = dQa)
  
  # OLS component of M
  mOLSi <- {y - Qa}*dQa
  
  # OLS component of M MT
  mmOLS <- sapply(X = 1L:n, 
                  FUN = function(i){ mOLSi[i,] %o% mOLSi[i,] }, 
                  simplify = "array")
  
  # derivative OLS component
  dmOLS <- - sapply(X = 1L:n, 
                    FUN = function(i){ dQa[i,] %o% dQa[i,] }, 
                    simplify = "array")
  
  # sum over individuals
  if (p == 1L) {
    mmOLS <- mean(x = mmOLS)
    dmOLS <- mean(x = dmOLS)
  } else {
    mmOLS <- rowMeans(x = mmOLS, dim = 2L)
    dmOLS <- rowMeans(x = dmOLS, dim = 2L)
  }
  
  # invert derivative OLS component
  invD <- solve(a = dmOLS) 
  
  return( list("MM" = mmOLS, "invdM" = invD) )
  
}


#------------------------------------------------------------------------------#
# outcome regression estimator of tx effect
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS
#  moOR : a modeling object specifying the outcome regression step
#  data : a data.frame containing covariates and tx
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#     fitOR : a modelObjFit object containing the results of the
#             outcome regression step
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the outcome regression estimator for the tx effect
#------------------------------------------------------------------------------#
delta_OR <- function(moOR, data, y) {
  
  #### Outcome Regression 
  
  fitOR <- modelObj::fit(object = moOR, data = data, response = y)
  
  # Q(X,0;betaHat)
  data$A <- 0L 
  Q0 <- drop(x = modelObj::predict(object = fitOR, newdata = data))
  
  # Q(X,1;betaHat)
  data$A <- 1L 
  Q1 <- drop(x = modelObj::predict(object = fitOR, newdata = data))
  
  #### Tx Effect
  
  EY <- array(data = 0.0, dim = 2L, dimnames = list(c("0","1")))
  EY[2L] <- mean(x = Q1)
  EY[1L] <- mean(x = Q0)
  delta <- unname(obj = EY[2L] - EY[1L])
  
  return( list(   "fitOR" = fitOR, 
                  "EY" = EY,
                  "deltaHat" = delta) )
}


#------------------------------------------------------------------------------#
# tx effect estimator component of the sandwich estimator 
# outcome regression estimator
#
# REQUIRES
#   delta_OR()
#
# ASSUMPTIONS
#   outcome regression model is a linear model
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS:
#  moOR : a modeling object specifying the outcome regression step
#         *** must be a linear model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# OUTPUTS:
#  a list containing
#  delta : the list returned by delta_OR()
#     MM : M M^T matrix
#   dMdB : matrix of derivatives of M wrt beta
#------------------------------------------------------------------------------#
delta_OR_swv <- function(moOR, data, y) {
  
  # estimate tx effect
  delta <- delta_OR(moOR = moOR, data = data, y = y)
  
  # Q(X,0;betaHat)
  data$A <- 0L 
  Q0 <- drop(modelObj::predict(object = delta$fitOR, newdata = data))
  
  # derivative of Q(X,0;betaHat)
  dQ0 <- stats::model.matrix(object = modelObj::model(object = moOR), 
                             data = data)
  
  # Q(X,1;betaHat)
  data$A <- 1L 
  Q1 <- drop(modelObj::predict(object = delta$fitOR, newdata = data))
  
  # derivative of Q(X,1;betaHat)
  dQ1 <- stats::model.matrix(object = modelObj::model(object = moOR), 
                             data = data)
  
  # delta component of M MT
  mmDelta <- mean(x = {Q1 - Q0 - delta$deltaHat}^2)
  
  # derivative of delta component w.r.t beta
  dMdB <- colMeans(x = dQ1 -dQ0) 
  
  return( list("delta" = delta, "MM" = mmDelta, "dMdB" = dMdB) )
  
}



#------------------------------------------------------------------------------#
# outcome regression estimator of tx effect and its standard error
#
# REQUIRES
#   swv_OLS() and delta_OR_swv()
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#   outcome regression model is a linear model and parameters are estimated 
#     using OLS
#
# INPUTS
#  moOR : a modeling object specifying the outcome regression step
#         *** must be a linear model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#  deltaHat : the outcome regression estimator for the tx effect
#        EY : the sample mean of the outcome under each tx option
#     fitOR : a modelObjFit object containing the results of the
#             outcome regression step
#  sigmaHat : the estimated standard error
#------------------------------------------------------------------------------#
delta_OR_se <- function(moOR, data, y) {
  
  #### OLS components
  OLS <- swv_OLS(mo = moOR, data = data, y = y) 
  
  #### estimator components
  OR <- delta_OR_swv(moOR = moOR, data = data, y = y)
  
  #### 1,1 Component of Sandwich Estimator
  
  # OLS contribution
  temp <- OR$dMdB %*% OLS$invdM
  sig11OLS <- temp %*% OLS$MM %*% t(x = temp)
  
  sig11 <- drop(x = OR$MM + sig11OLS)
  
  return( c(OR$delta, "sigmaHat" = sqrt(x = sig11 / nrow(x = data))) )
  
}


#------------------------------------------------------------------------------#
# stratification estimator of tx effect
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS
#  moPS : a modeling object specifying the propensity score regression step
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#     K : the integer number of strata
#
# RETURNS
#  a list containing
#      fitPS : a modelObjFit object containing the results of the
#              propensity score regression step
#         cj : the propensity score boundaries defining the strata
#        grp : the number of individuals in each stratum
#         EY : the sample mean of the outcome under each tx option
#  deltaHatj : the stratification estimator for the tx effect for each strata
#   deltaHat : the stratification estimator for the tx effect
#------------------------------------------------------------------------------#
delta_S <- function(moPS, data, y, K) {
  
  #### Propensity Score Regression
  
  fitPS <- modelObj::fit(object = moPS, data = data, response = data$A)
  
  # estimated propensity score
  p1 <- modelObj::predict(object = fitPS, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  #### Identify Strata
  
  # cutoff points for K groups 
  probs <- seq(from = 0.0, to = 1.0, length.out = K+1L)
  coff <- stats::quantile(x = p1, probs = probs) 
  coff[1L] <- 0.0
  coff[K+1L] <- 1.0
  
  # group ids for each individual
  grp <- colSums(x = sapply(X = p1, FUN = function(x){x <= coff}))
  
  #### Treatment Effect
  
  EY <- matrix(data = 0.0, nrow = K, ncol = 2L, 
               dimnames = list(NULL, c("0","1")))
  
  delta <- 0.0
  
  deltaj <- array(data = 0.0, dim = K, dimnames = list(1L:K))
  
  for (j in 1L:K) {
    
    gji <- grp == j
    
    EY[j,2L] <- mean(x = data$A[gji] * y[gji]) / mean(x = data$A[gji])
    EY[j,1L] <- mean({1.0-data$A[gji]} * y[gji]) / mean(x = 1.0 - data$A[gji])
    
    deltaj[j] <- EY[j,2L] - EY[j,1L]
    
    delta <- delta + sum(gji) / nrow(x = data) * deltaj[j]
  }
  
  delta <- unname(obj = delta)
  
  return( list(    "fitPS" = fitPS,
                   "cj" = coff,
                   "grp" = table(bin = grp),
                   "EY" = EY,
                   "deltaHatj" = deltaj,
                   "deltaHat" = delta) )
  
}



#------------------------------------------------------------------------------#
# stratification estimator of tx effect and its standard error
#
# REQUIRES
#   delta_S()
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS
#  moPS : a modeling object specifying the propensity score regression step
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#     K : the integer number of strata
#
# RETURNS
#  a list containing
#      fitPS : a modelObjFit object containing the results of the
#              propensity score regression step
#         cj : the propensity score boundaries defining the strata
#        grp : the number of individuals in each stratum
#         EY : the sample mean of the outcome under each tx option
#  deltaHatj : the stratification estimator for the tx effect for each strata
#   deltaHat : the stratification estimator for the tx effect
#   sigmaHat : the estimated standard error
#------------------------------------------------------------------------------#
delta_S_se <- function(moPS, data, y, K) {
  
  #### Treatment Effect
  
  delta <- delta_S(moPS = moPS, data = data, y = y, K = K)
  
  #### Standard Error
  
  # estimated propensity score
  p1 <- modelObj::predict(object = delta$fitPS, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  # group ids for each individual
  grp <- colSums(x = sapply(X = p1, FUN = function(x){x <= delta$cj}))
  
  sigmaSq <- 0.0
  
  for (j in 1L:K) {
    
    gji <- grp == j
    
    Y1ji <- data$A[gji] * y[gji] / mean(x = data$A[gji])
    Y0ji <- {1.0-data$A[gji]} * y[gji] / mean(x = 1.0 - data$A[gji])
    
    deltaji <- {Y1ji - Y0ji}
    
    sigmaSq <- sigmaSq + mean(x = {deltaji - delta$deltaHatj[j]}^2)
  }
  
  sigmaSq <- sigmaSq / K
  
  return( c(delta, "sigmaHat" = sqrt(x = sigmaSq / nrow(x = data))) )
  
}


#------------------------------------------------------------------------------#
# components of the sandwich estimator for a maximum likelihood estimation of 
#   a logistic regression model
#
# ASSUMPTIONS
#   mo is a logistic model with parameters to be estimated using ML
#
# INPUTS
#    mo : a modeling object specifying a regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#     y : a vector containing the binary outcome of interest
#
#
# RETURNS
# a list containing
#     MM : M^T M component from ML
#  invdM : inverse of the matrix of derivatives of M wrt gamma
#------------------------------------------------------------------------------#
swv_ML <- function(mo, data, y) {
  
  # regression step
  fit <- modelObj::fit(object = mo, data = data, response = y)
  
  # yHat
  p1 <- modelObj::predict(object = fit, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  # model.matrix for model
  piMM <- stats::model.matrix(object = modelObj::model(object = mo), 
                              data = data)
  
  n <- nrow(x = piMM)
  p <- ncol(x = piMM)
  
  # ML M-estimator component
  mMLi <- {y - p1} * piMM
  
  # ML component of M MT
  mmML <- sapply(X = 1L:n, 
                 FUN = function(i){ mMLi[i,] %o% mMLi[i,] }, 
                 simplify = "array")
  
  # derivative of ML component
  dFun <- function(i){ 
    - piMM[i,] %o% piMM[i,] * p1[i] * {1.0 - p1[i]}
  } 
  dmML <- sapply(X = 1L:n, FUN = dFun, simplify = "array")
  
  if( p == 1L ) {
    mmML <- mean(x = mmML)
    dmML <- mean(x = dmML)
  } else {
    mmML <- rowMeans(x = mmML, dim = 2L)
    dmML <- rowMeans(x = dmML, dim = 2L)
  }
  
  # invert derivative ML component
  invD <- solve(a = dmML) 
  
  return( list("MM" = mmML, "invdM" = invD) )
  
}

#------------------------------------------------------------------------------#
# IPW estimator of tx effect 
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS
#  moPS : a modeling object specifying the propensity score regression step
#  data : a data.frame containing covariates and tx
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#     fitPS : a modelObjFit object containing the results of the
#             propensity score regression step
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the outcome regression estimator for the tx effect
#------------------------------------------------------------------------------#
delta_IPW <- function(moPS, data, y) {
  
  #### Propensity Score
  
  fitPS <- modelObj::fit(object = moPS, data = data, response = data$A)
  
  # estimated propensity score
  p1 <- modelObj::predict(object = fitPS, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  #### Treatment Effect
  
  EY <- array(data = 0.0, dim = 2L, dimnames = list(c("0","1")))
  EY[1L] <- mean(x = {1.0 - data$A} * y / {1.0 - p1})
  EY[2L] <- mean(x = data$A * y / p1) 
  delta <- unname(obj = EY[2L] - EY[1L])
  
  return( list(   "fitPS" = fitPS,
                  "EY" = EY,
                  "deltaHat" = delta) )
}


#------------------------------------------------------------------------------#
# tx effect estimator component of the sandwich estimator IPW estimator
#
# REQUIRES
#   delta_IPW()
#
# ASSUMPTIONS
#   propensity score regression model is a logistic model
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS:
#  moPS : a modeling object specifying the propensity score regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# OUTPUTS:
#  a list containing
#  delta : the list returned by delta_IPW()
#     MM : M M^T matrix
#   dMdG : matrix of derivatives of M wrt gamma
#------------------------------------------------------------------------------#
delta_IPW_swv <- function(moPS, data, y) {
  
  # estimate treatment effect
  delta <- delta_IPW(moPS = moPS, data = data, y = y)
  
  # pi(x; gamma)
  p1 <- modelObj::predict(object = delta$fitPS, newdata = data)
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  # model.matrix for propensity model
  piMM <- stats::model.matrix(object = modelObj::model(object = moPS), 
                              data = data)
  
  # delta component of M MT
  mmDelta <- mean(x = {data$A * y / p1 - {1.0-data$A} * y / {1.0-p1} - 
      delta$deltaHat}^2)
  
  # derivative w.r.t. gamma
  dMdG <- -{ data$A * y * {1.0-p1} / p1 +
      {1.0-data$A} * y * p1 / {1.0-p1} } * piMM
  dMdG <- colMeans(x = dMdG)
  
  return( list("delta" = delta,
               "MM" = mmDelta,
               "dMdG" = dMdG) )
  
}


#------------------------------------------------------------------------------#
# IPW estimator of the tx effect and its standard error
#
# REQUIRES
#   swv_ML() and delta_IPW_swv()
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#   propensity score regression model is a logistic model and parameters are
#     estimated using ML
#
# INPUTS
#  moOR : a modeling object specifying the propensity score regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#     fitPS : a modelObjFit object containing the results of the
#             propensity score regression step
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the outcome regression estimator for the tx effect
#  sigmaHat : the estimated standard error
#------------------------------------------------------------------------------#
delta_IPW_se <- function(moPS, data, y){
  
  #### ML components
  ML <- swv_ML(mo = moPS, data = data, y = data$A) 
  
  #### estimator components
  IPW <- delta_IPW_swv(moPS = moPS, data = data, y = y)
  
  #### 1,1 Component of Sandwich Estimator
  
  # ML contribution
  temp <- IPW$dMdG %*% ML$invdM
  sig11ML <- temp %*% ML$MM %*% t(x = temp)
  
  sig11 <- drop(x = IPW$MM + sig11ML)
  
  return( c(IPW$delta, "sigmaHat" = sqrt(x = sig11 / nrow(x = data))) )
  
}



#------------------------------------------------------------------------------#
# components of the sandwich estimator for an ordinary least squares estimation
#   of a linear regression model
#
# ASSUMPTIONS
#   mo is a linear model with parameters to be estimated using OLS
#
# INPUTS
#    mo : a modeling object specifying a regression step
#         *** must be a linear model ***
#  data : a data.frame containing covariates and tx
#     y : a vector containing the outcome of interest
#
#
# RETURNS
# a list containing
#     MM : M^T M component from OLS
#  invdM : inverse of the matrix of derivatives of M wrt beta
#------------------------------------------------------------------------------#
swv_OLS <- function(mo, data, y) {
  
  n <- nrow(x = data)
  
  fit <- modelObj::fit(object = mo, data = data, response = y)
  
  # Q(X,A;betaHat)
  Qa <- drop(x = modelObj::predict(object = fit, newdata = data))
  
  # derivative of Q(X,A;betaHat)
  dQa <- stats::model.matrix(object = modelObj::model(object = mo), 
                             data = data)
  
  # number of parameters in model
  p <- ncol(x = dQa)
  
  # OLS component of M
  mOLSi <- {y - Qa}*dQa
  
  # OLS component of M MT
  mmOLS <- sapply(X = 1L:n, 
                  FUN = function(i){ mOLSi[i,] %o% mOLSi[i,] }, 
                  simplify = "array")
  
  # derivative OLS component
  dmOLS <- - sapply(X = 1L:n, 
                    FUN = function(i){ dQa[i,] %o% dQa[i,] }, 
                    simplify = "array")
  
  # sum over individuals
  if (p == 1L) {
    mmOLS <- mean(x = mmOLS)
    dmOLS <- mean(x = dmOLS)
  } else {
    mmOLS <- rowMeans(x = mmOLS, dim = 2L)
    dmOLS <- rowMeans(x = dmOLS, dim = 2L)
  }
  
  # invert derivative OLS component
  invD <- solve(a = dmOLS) 
  
  return( list("MM" = mmOLS, "invdM" = invD) )
  
}


#------------------------------------------------------------------------------#
# components of the sandwich estimator for a maximum likelihood estimation of 
#   a logistic regression model
#
# ASSUMPTIONS
#   mo is a logistic model with parameters to be estimated using ML
#
# INPUTS
#    mo : a modeling object specifying a regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#     y : a vector containing the binary outcome of interest
#
#
# RETURNS
# a list containing
#     MM : M^T M component from ML
#  invdM : inverse of the matrix of derivatives of M wrt gamma
#------------------------------------------------------------------------------#
swv_ML <- function(mo, data, y) {
  
  # regression step
  fit <- modelObj::fit(object = mo, data = data, response = y)
  
  # yHat
  p1 <- modelObj::predict(object = fit, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  # model.matrix for model
  piMM <- stats::model.matrix(object = modelObj::model(object = mo), 
                              data = data)
  
  n <- nrow(x = piMM)
  p <- ncol(x = piMM)
  
  # ML M-estimator component
  mMLi <- {y - p1} * piMM
  
  # ML component of M MT
  mmML <- sapply(X = 1L:n, 
                 FUN = function(i){ mMLi[i,] %o% mMLi[i,] }, 
                 simplify = "array")
  
  # derivative of ML component
  dFun <- function(i){ 
    - piMM[i,] %o% piMM[i,] * p1[i] * {1.0 - p1[i]}
  } 
  dmML <- sapply(X = 1L:n, FUN = dFun, simplify = "array")
  
  if( p == 1L ) {
    mmML <- mean(x = mmML)
    dmML <- mean(x = dmML)
  } else {
    mmML <- rowMeans(x = mmML, dim = 2L)
    dmML <- rowMeans(x = dmML, dim = 2L)
  }
  
  # invert derivative ML component
  invD <- solve(a = dmML) 
  
  return( list("MM" = mmML, "invdM" = invD) )
  
}



#------------------------------------------------------------------------------#
# doubly robust estimator of tx effect 
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS
#  moOR : a modeling object specifying the outcome regression step
#  moPS : a modeling object specifying the propensity score regression step
#  data : a data.frame containing covariates and tx
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#     fitOR : a modelObjFit object containing the results of the
#             outcome regression step
#     fitPS : a modelObjFit object containing the results of the
#             propensity score regression step
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the doubly robust estimator for the tx effect
#------------------------------------------------------------------------------#
delta_DR <- function(moOR, moPS, data, y) {
  
  #### Propensity Score
  
  fitPS <- modelObj::fit(object = moPS, data = data, response = data$A)
  
  # estimated propensity score
  p1 <- modelObj::predict(object = fitPS, newdata = data) 
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  #### Outcome Regression 
  
  fitOR <- modelObj::fit(object = moOR, data = data, response = y)
  
  # store tx variable
  A <- data$A 
  
  # estimated Q-function when all A=0
  data$A <- 0L 
  Q0 <- drop(x = modelObj::predict(object = fitOR, newdata = data))
  
  # estimated Q-function when all A=1
  data$A <- 1L 
  Q1 <- drop(x = modelObj::predict(object = fitOR, newdata = data))
  
  #### Treatment Effect
  
  EY <- array(data = 0.0, dim = 2L, dimnames = list(c("0","1")))
  aug <- {A - p1} * {Q1 / p1 + Q0 / {1.0 - p1}}
  EY[2L] <- mean(x = {A == 1L} * {y / p1 - aug})
  EY[1L] <- mean(x = {A == 0L} * {y / {1.0 - p1} + aug})
  delta <- unname(obj = EY[2L] - EY[1L])
  
  return( list(   "fitOR" = fitOR,
                  "fitPS" = fitPS,
                  "deltaHat" = delta,
                  "EY" = EY) )
  
}


#------------------------------------------------------------------------------#
# tx effect estimator component of the sandwich estimator 
# doubly robust estimator
#
# REQUIRES
#   delta_DR()
#
# ASSUMPTIONS
#   outcome regression model is a linear model
#   propensity score regression model is a logistic model
#   tx is denoted by A and is binary coded as {0,1}
#
# INPUTS:
#  moOR : a modeling object specifying the outcome regression step
#         *** must be a linear model ***
#  moPS : a modeling object specifying the propensity score regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# OUTPUTS:
#  a list containing
#  delta : the list returned by delta_DR()
#     MM : M M^T matrix
#   dMdB : matrix of derivatives of M wrt beta
#   dMdG : matrix of derivatives of M wrt gamma
#------------------------------------------------------------------------------#
delta_DR_swv <- function(moOR, moPS, data, y) {
  
  # estimate treatment effect
  delta <- delta_DR(moOR = moOR, moPS = moPS, data = data, y = y)
  
  # pi(x;gammaHat)
  p1 <- modelObj::predict(object = delta$fitPS, newdata = data)
  if (is.matrix(x = p1)) p1 <- p1[,ncol(x = p1), drop = TRUE]
  
  # model.matrix for propensity model
  piMM <- stats::model.matrix(object = modelObj::model(object = moPS), 
                              data = data)
  
  A <- data$A
  
  # Q(x,A=0;betaHat)
  data$A <- 0L 
  Q0 <- drop(modelObj::predict(object = delta$fitOR, newdata = data))
  
  # dQ(x,A=0;betaHat)
  # derivative of a linear model is the model.matrix
  dQ0 <- stats::model.matrix(object = modelObj::model(object = moOR), 
                             data = data)
  
  # Q(x,A=1;betaHat)
  data$A <- 1L 
  Q1 <- drop(modelObj::predict(object = delta$fitOR, newdata = data))
  
  # dQ(x,A=1;betaHat)
  # derivative of a linear model is the model.matrix
  dQ1 <- stats::model.matrix(object = modelObj::model(object = moOR), 
                             data = data)
  
  data$A <- A
  
  # delta component of M-equation
  mDeltai <- data$A*y/p1 - {1.0 - data$A}*y/{1.0 - p1} - 
    {data$A - p1}*{Q1/p1 + Q0/{1.0 - p1}} - delta$deltaHat
  
  mmDelta <- mean(x = mDeltai^2)
  
  # derivative of delta component w.r.t. beta
  dMdB <- colMeans(x = {data$A - p1}*{dQ1/p1 + dQ0/{1.0 - p1}})
  
  # derivative of delta component w.r.t. gamma
  dMdG <- - data$A/p1^2*{y - Q1} - {1.0 - data$A}/{1.0 - p1}^2*{y - Q0} 
  dMdG <- colMeans(x = dMdG*p1*{1.0 - p1}*piMM) 
  
  return( list("delta" = delta,
               "MM" = mmDelta,
               "dMdB" = dMdB,
               "dMdG" = dMdG) )
  
}


#------------------------------------------------------------------------------#
# doubly robust estimator of tx effect and its standard error
#
# REQUIRES
#   swv_ML(), swv_OLS(), and delta_DR_swv()
#
# ASSUMPTIONS
#   tx is denoted by A and is binary coded as {0,1}
#   outcome regression model is a linear model and parameters are estimated 
#     using OLS
#   propensity score regression model is a logistic model and parameters are
#     estimated using ML
#
# INPUTS
#  moOR : a modeling object specifying the outcome regression step
#         *** must be a linear model ***
#  moPS : a modeling object specifying the propensity score regression step
#         *** must be a logistic model ***
#  data : a data.frame containing covariates and tx
#         *** tx must be named 'A' and coded 0/1 ***
#     y : a vector containing the outcome of interest
#
# RETURNS
#  a list containing
#     fitOR : a modelObjFit object containing the results of the
#             outcome regression step
#     fitPS : a modelObjFit object containing the results of the
#             propensity score regression step
#        EY : the sample mean of the outcome under each tx option
#  deltaHat : the doubly robust estimator for the tx effect
#  sigmaHat : the estimated standard error
#------------------------------------------------------------------------------#
delta_DR_se <- function(moOR, moPS, data, y) {
  
  #### ML components
  ML <- swv_ML(mo = moPS, data = data, y = data$A) 
  
  #### OLS components
  OLS <- swv_OLS(mo = moOR, data = data, y = y) 
  
  #### estimator components
  DR <- delta_DR_swv(moOR = moOR, moPS = moPS, data = data, y = y)
  
  #### 1,1 Component of Sandwich Estimator
  
  # ML contribution
  temp <- DR$dMdG %*% ML$invdM
  sig11ML <- temp %*% ML$MM %*% t(x = temp)
  
  # OLS contribution
  temp <- DR$dMdB %*% OLS$invdM
  sig11OLS <- temp %*% OLS$MM %*% t(x = temp)
  
  sig11 <- drop(x = DR$MM + sig11ML + sig11OLS)
  
  return( c(DR$delta, "sigmaHat" = sqrt(x = sig11 / nrow(x = data))) )
  
}