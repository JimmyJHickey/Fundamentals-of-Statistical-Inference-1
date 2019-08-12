#####################################################################
##Demonstration of WLLN to do integration
##i.e. Monte Carlo Integration examples
##Justin Post - Fall 2015
#####################################################################

##########################################
#X~N(0,1), estimate I(g(x))=P(0<X<1)
#Generate n  values from U(0,1)
n<-10000
x<-runif(n)

#Evaluate all x's at g
gx<-(1/sqrt(2*pi))*exp(-x^2/2)
#Approximate by averaging, which converges to truth by WLLN, i.e. converges to I(g(x))=P(0<X<1)
approximation<-sum(gx)/n

#compare approximation to truth
approximation
#truth
pnorm(q=1)-pnorm(q=0)
#0.3413

##########################################
#Estimation of g=4-x^2-y^2-z^2 from z=0,11/10, y=0,1, and x=0,9/10
#generate random values from appropriate ranges 
xyz<-data.frame(x=runif(n,min=0,max=9/10),y=runif(n,min=0,max=1),z=runif(n,min=0,max=11/10))

#evaluate at g
g<-4-xyz$x^2-xyz$y^2-xyz$z^2

#Approximate by averaging with appropriate scaling
(9/10)*(1)*(11/10)*mean(g)

#Truth is 2.9634