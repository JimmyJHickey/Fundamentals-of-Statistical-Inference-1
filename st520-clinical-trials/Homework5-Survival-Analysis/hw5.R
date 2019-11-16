###
# Jimmy Hickey  
# 2019-11-15
# ST520 Homework 5 On Survival Analysis
###


###
# 4
###

survival_times = data.frame(
  "surv time" = c(1.43, 0.95, 3.89, 2.25, 0.73, 3.24, 2.30, 1.98, 4.03, 3.30, 4.20, 1.58), 
  "failure" = c(1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1)
  )


###
# 5
# Plot length of study as a function of accrual rate.
# This code is modified from that of Dr. Shu Yang.
###



accrual  = NULL   
study_length = NULL 

# accrual rate of 200 patients per year
accrual_rate <- 200 #a(u)

# standard median survival is 5 years
median_standard = 5

# median we want to detect is 7 years
median_treatment = 7

# hazard ratios for treatment and standard
hr_treatment<-log(2)/median_treatment

hr_standard<-log(2)/median_standard

# death to detect for power 0.9, alpha = 0.05, and 3 treatments
deaths = 672


f<-function(a,l){
  .5*rate*(a-exp(-hr_treatment*l)*(exp(hr_treatment*a)-1)/hr_treatment) +
     .5*rate*(a-exp(-hr_standard*l)*(exp(hr_standard*a)-1)/hr_standard)-deaths
}

# min length of study
# max accrual
# Acc=L
f1<-function(x){
  f(x,x)
}

z<-uniroot(f1,c(1,100))
upper<-z$root  
upper


f2<-function(y){f(10,y)}
uniroot(f2,c(1,50))$root

# minimum accrual
# accrual required deaths and stop accrual immediately 
lower<-deaths/rate

n<-100  
for (i in 1:n){
  accrual[i]<-lower+i*(upper-lower)/n
  f3<-function(y){f(accrual[i], y)}
  v<-uniroot(f3,c(1,50))
  study_length[i]<-v$root
}
plot(accrual , study_length,type='l',xlab='accrual (years)',ylab='length of study (years)')



###
# 6
###

# Read calrisk data
# variable descriptions
# V1:   days on study
# V2:   failure indicator (1 = death, 0 = censored)
# V3:   treatment indicator (1 = treatment 3 [Standard dose CAF], 0 = treatment 2 [Low dose CAF])
# risk: risk indicator
calrisk = read.table('Homework5-Survival-Analysis/calrisk.dat', header=TRUE)


# a.
# Compare survival curves between treatments given risk indicator
# perform 2 sample log rank test

library(survival)

days = calrisk$V1
cens = calrisk$V2
trt = calrisk$V3
years = days / 365.25
risk = calrisk$risk

km <- survfit(Surv(days, cens)~risk)
summary(km)




plot(km,xlab="years",ylab="prob",lty=1:2,col=1:2)
legend(2, .4,  c("risk=0",
              "risk=1"),
       lty=1:2,col=1:2)


lr <- survdiff(Surv(days,cens)~risk)
lr
# Chisq= 8.3  on 1 degrees of freedom, p= 0.00391 


# Stratify by treatment

# treatment 0

km_treat0 = survfit(Surv(days, cens)~risk, subset = (trt==0))

plot(km_treat0,xlab="years",ylab="prob",lty=1:2,col=1:2)
legend(2, .4,  c("risk=0",
                 "risk=1"),
       lty=1:2,col=1:2)

lr_treat0 <- survdiff(Surv(days, cens)~risk, subset = (trt==0))
lr_treat0
# Chi sq = 0.3, p = 0.601

# treatment 1

km_treat1 <- survfit(Surv(days, cens)~risk, subset = (trt==1))

plot(km_treat1,xlab="years",ylab="prob",lty=1:2,col=1:2)
legend(2, .4,  c("risk=0",
                 "risk=1"),
       lty=1:2,col=1:2)

lr_treat1 <- survdiff(Surv(days, cens)~risk, subset = (trt==1))
lr_treat1
# Chisq= 12.7  on 1 degrees of freedom, p= 0.000366