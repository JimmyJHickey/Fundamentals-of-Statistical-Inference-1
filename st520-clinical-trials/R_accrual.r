
x<-NULL   #x is Acc
y<-NULL   #y is L
rate<-100 #a(u)
hr1<-log(2)/6#.116 
hr2<-log(2)/4#.173

f<-function(a,l){
  .5*rate*(a-exp(-hr1*l)*(exp(hr1*a)-1)/hr1)+.5*rate*(a-exp(-hr2*l)*(exp(hr2*a)-1)/hr2)-256
}

#min length of study
#max accrual
#Acc=L
f1<-function(x){
  f(x,x)
}
z<-uniroot(f1,c(1,100))
upper<-z$root  
upper


f2<-function(y){f(5,y)}
uniroot(f2,c(1,50))$root

#minimum accrual
#accrual d=256 and stop accrual immediately 
lower<-256/rate

n<-100  
for (i in 1:n){
  x[i]<-lower+i*(upper-lower)/n
  f3<-function(y){f(x[i],y)}
  v<-uniroot(f3,c(1,50))
  y[i]<-v$root
}
plot(x,y,type='l',xlab='accrual (years)',ylab='length of study (years)')

