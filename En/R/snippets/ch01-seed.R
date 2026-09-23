# Random numbers grow deterministically from a seed. With a fixed seed they repeat.
set.seed(1); a <- rnorm(3)
set.seed(1); b <- rnorm(3)
identical(a, b)
round(rbind(a = a, b = b), 4)

set.seed(2); round(rnorm(3), 4)               # a different seed, a different sequence
round(rnorm(3), 4)                            # no reseeding: the sequence continues

# The rule of this book: every snippet that uses random numbers sets its own seed, and
# that code stays in the same repository as the results. Results alone cannot be redone.
