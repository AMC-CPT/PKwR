# Functions: every model in this book is expressed as a single function
decay <- function(t, A = 100, k = 0.25) A*exp(-k*t)
round(decay(c(0, 2, 4)), 3)                   # arguments with defaults can be omitted
round(decay(2, k = 0.5), 3)                   # passing by name frees the order

# The last expression is the return value; return several things as a list or a vector.
summ <- function(v) c(n = length(v), mean = mean(v), sd = sd(v))
round(summ(decay(seq(0, 10, 2))), 3)

# A function returning a function (closure): stamps out new functions with k fixed.
maker <- function(k) function(t) decay(t, k = k)
fast <- maker(1.0); slow <- maker(0.1)
round(c(fast.t2 = fast(2), slow.t2 = slow(2)), 3)
