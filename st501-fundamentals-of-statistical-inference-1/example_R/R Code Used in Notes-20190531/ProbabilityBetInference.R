###################################################
##R file to simulate betting on 10 games
##Justin Post
###################################################

#set seed to make example repeatable
set.seed(12)
#Create true probability of winning
p <- 0.8

#Create an example sequence of 10 bets (1 means win, 0 loss)
bet<-rbinom(n=10,size=1,prob=p)
bet
sum(bet)

#or do with size=10
rbinom(n=1,size=10,prob=p)

#repeat 10 bets 100 times
bets<-rbinom(n=100,size=10,prob=p)
bets

#see how often 5 comes up
(bets==5)
sum(bets==5)/100

#up the number of times we run the experiment to get closer to actual probability
bets<-rbinom(n=10000,size=10,prob=p)
sum(bets==5)/10000

#Is our outcome of 5 wins reasonable if we assume an 80% probabilty of success?
#create visual
hist(bets, col = "red")
#add line for our observed value
abline(v=5,lwd=2)
#color "more extreme values"
h <- hist(bets, plot = F)
cuts <- cut(h$breaks, c(-Inf, 5, Inf))
plot(h, col = cuts)
abline(v=5,lwd=2)
text(3.25,2000, paste0("Probability of 5 or less is ", round(pbinom(size=10,prob=p,5),4)))

#empirical (data driven) probability of being 5 or less
mean(bets <= 5)
