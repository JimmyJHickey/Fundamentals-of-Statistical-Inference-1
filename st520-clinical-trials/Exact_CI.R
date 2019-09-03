n=19
k=3

f=function(p){
  pbinom(3,19,p)-0.025
}

piU<-uniroot(f,c(0,1))$root
piU
# Exact upper bound is 0.3957844

g=function(p){
  # Remember to subtract 1 at the beginning for lower endpoint
  pbinom(2,19,p)-0.975
}

piL<-uniroot(g,c(0,1))$root
piL
# Exact lower bound is 0.03380492

c(piL,piU)

# Or use a Hmisc package
library(Hmisc)
binconf(k,n,alpha=.05,method="all")

