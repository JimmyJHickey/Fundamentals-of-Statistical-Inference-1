###
#
# ST705 Final Exam Code
# Jimmy Hickey
# 2020-05-03
###

library(MASS)

# 1

calc_h = function(Kt, X)
{
 H = Kt %*% ginv( t(X) %*% X ) %*% t(Kt)
 return(H)
}

calc_power = function(slope){
 b = c(0, slope)
 X = cbind( rep(1, 5), seq(1,5))
 Kt = matrix(c(
 0, 1
 ), byrow=T, ncol=2)
 m=0
 
 H = calc_h(Kt, X)
 
 phi = 1/2 * t( Kt %*% b - m ) %*% solve(H) %*% (Kt %*% b - m)
 
 F_05 = qf(1 - 0.05, 1, 3)
 
 power = 1 - pf(F_05, 1, 3, phi)
 
 return(power)
}

calc_power(0.1)
calc_power(0.2)
calc_power(0.3)
