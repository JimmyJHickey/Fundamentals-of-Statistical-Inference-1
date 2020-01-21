seq_length = 1000
lower = 0
upper = 1

sequence = runif(seq_length, lower, upper)

sums = cumsum(sequence)

plot(sums)
