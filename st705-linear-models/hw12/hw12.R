library(MASS)

calc_h = function(Kt, X)
{
 H = Kt %*% ginv( t(X) %*% X ) %*% t(Kt)
 return(H)
}

qt(0.05/6, df = 15-3)
# -2.779473

X = cbind( rep(1, 15), 
           c(rep(1,5), rep(0, 10)),
           c(rep(0,5), rep(1,5), rep(0, 5)),
           c(rep(0,10), rep(1, 5))
             )
X

K = matrix(c(
 0, 1 , -1 , 0 ,
 0, 1 , 0 , -1 ,
 0, 0 , 1, -1
), byrow=T, ncol=4)


X = cbind( rep(1, 15), 
           c(rep(1,5), rep(0, 10)),
           c(rep(0,5), rep(1,5), rep(0, 5)),
           c(rep(0,10), rep(1, 5))
)
X

# Naomi said I couldn't make this with reps
Kt = cbind( rep(0,3),
 c(rep(1,2), rep(0,1)),
 c(rep(-1,1), rep(0,1), rep(1,1)),
 c(rep(0,1), rep(-1,2))
)

Kt

H = Kt %*% ginv( t(X) %*% X ) %*% t(Kt)
H

# Scheffe
2 * sqrt(3 * qf(0.05, 3, 15-3, lower.tail = FALSE) )


# Tukey
2/sqrt(5) * qtukey(0.05, 3, 5*(3-1))



# 6.19

calc_h = function(Kt, X)
{
 H = Kt %*% ginv( t(X) %*% X ) %*% t(Kt)
 return(H)
}

b = c(0, 1,1,0)


X = cbind( rep(1, 15), 
           c(rep(1,5), rep(0, 10)),
           c(rep(0,5), rep(1,5), rep(0, 5)),
           c(rep(0,10), rep(1, 5))
)

## a
Kt = matrix(c(
 0, 1 , -1 , 0 ,
 0, 1 , 0 , -1
), byrow=T, ncol=4)
m=c(0,0)


H = calc_h(Kt, X)

phi = 1/2 * t( Kt %*% b - m ) %*% solve(H) %*% (Kt %*% b - m)
phi


# b
Kt = matrix(c(
 0, 1 , 0 , -1,
 0, 0 , 1 , -1
), byrow=T, ncol=4)
m=c(0,0)


H = calc_h(Kt, X)

phi = 1/2 * t( Kt %*% b - m ) %*% solve(H) %*% (Kt %*% b - m)
phi


## c
qt(0.05/4, 12, lower.tail = FALSE)

Kt = matrix(c(
 0, 1 , -1 , 0 ,
 0, 1 , 0 , -1
), byrow=T, ncol=4)


mu = (Kt %*% b) / (sqrt( Kt %*% ginv( t(X) %*% X) %*% t(Kt))[1,1])
mu

## d
Kt = matrix(c(
 0, 1 , 0 , -1,
 0, 0 , 1 , -1
), byrow=T, ncol=4)

mu = (Kt %*% b) / (sqrt( Kt %*% ginv( t(X) %*% X) %*% t(Kt))[1,1])
mu


# 6.25

get_PX = function(X)
{
 return( X %*% ginv( t(X) %*% X ) %*% t(X) )
}

X = matrix(c(
 1, 1, 0, 0,
 1, 0, 1, 0,
 1, 0, 0, 1,
 0, 1, 1, 0,
 0, 1, 0, 1,
 0, 0, 1, 1
), byrow=T, ncol=4)

Px = get_PX(X)

I = diag(6)

y = c(-1,3,2,4,3,1)

sigma_hat_sq = t(y) %*% (I- Px) %*% y /(2)

sigma_hat_sq

# b

m=c(0,0,0)

Kt = 
 matrix(c(
  1, -1, 0, 0,
  0, 1, -1, 0,
  0, 0, 1, -1
 ), byrow=T, ncol=4)

bhat = c(0,1,2,1)

H = calc_h(Kt, X)

F = ( t( Kt %*% bhat - m) %*% solve(H) %*% 
       (Kt %*% bhat - m)) / 3 /sigma_hat_sq
F


## d

zhat = (t(y) %*% X %*% c) / (t(c) %*% t(X) %*% X %*% c)
zhat




## e
K = 
  matrix(c(
    1, 1, 1,
    -1, 0, 0,
    0, -1, 0,
    0, 0, -1
  ), byrow=T, ncol=3)
c = c(1,1,1,1)

theta_hat= solve(t(K) %*% K) %*%
 (t(K) %*% t(X) %*% y - 
   t(K) %*% t(X) %*% X %*% c)

theta_hat


# 8.16

X =  matrix(c(
 1, 1,
 1, 2,
 1, 3,
 1, 4,
 1, 1,
 1, 2,
 1, 3,
 1, 4, 
 1, 1,
 1, 2,
 1, 3,
 1, 4
), byrow=T, ncol=2)

Px = get_PX(X)

I12 = diag(12)

(I12 - Px) %*% X


Z =  matrix(c(
 1, 0, 0,
 1, 0, 0,
 1, 0, 0,
 1, 0, 0,
 0, 1, 0,
 0, 1, 0,
 0, 1, 0,
 0, 1, 0, 
 0, 0, 1,
 0, 0, 1,
 0, 0, 1,
 0, 0, 1
), byrow=T, ncol=3)

Pz = get_PX(Z)

one_vec = rep(1,12)

P1 = get_PX(one_vec)

(Pz - P1) %*% X

sum(diag(I12 - Px))
sum(diag(Pz - P1))
sum(diag( t(Z) %*% (I12 - Px) %*% Z ))
sum(diag(t(Z) %*% (Pz - P1) %*% Z))
