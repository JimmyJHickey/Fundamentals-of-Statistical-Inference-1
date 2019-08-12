###
# Jimmy Hickey
# 2019-05-18
# ST501
# Simulation Ideas Using R
###

##
# Ex: Suppose we bet on 10 games and we assume 
# we have an 80% chance of winning each bet placed.
# Define A = { Win 5 bets }.

# Assuming we bet on 10 games and won exactly 5, is it reasonable to assume
# that I have at least 80% chance of winning a bet?

my_seed = 12
set.seed(my_seed)

# True probability of winning
p = 0.8

# Create an examople of 10 bets (1 means win, 0 loss)
# rbinom generates a random binomial value
bet = rbinom(n = 10, size = 1, prob = p)
print(bet)
print(sum(bet))

# or
rbinom(n = 1, size = 10, prob = p)

# repeat 10 bets 100 times
bets = rbinom(n = 100, size = 10, prob = p)
print(bets)

# see how often 5 comes up
(bets == 5)
sum(bets == 5) / 100

# up the number of runs to get more realistic 
bets = rbinom(n = 10000, size = 10, prob = p)
sum(bets==5)/ 10000

# Is our outcome of 5 wins reasonable if we assume an 80% probability of success?
hist(bets, col = "red")

# add line for our observed value
abline(v = 5, lwd = 2)

# color more "extreme" values
h = hist(bets, plot = F)
cuts = cut(h$breaks, c(-Inf, 5, Inf))
plot(h, col = cuts)
abline(v = 5, lwd = 2)
text(4, 2000, paste0("Probability of 5 or less is ",
                        round(pbinom(size =10, prob = p, 5), 4)))
