seq_length = 1000
lower = 0
upper = 1

sequence = runif(seq_length, lower, upper)

sums = vector()

for (i in 1:seq_length)
{
  sums[i] = sum(sequence[1:i])
}

plot(sums)
