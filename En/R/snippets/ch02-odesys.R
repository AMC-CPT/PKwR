# Linear system dy/dt = K y. The solution: a sum of exponentials set by K's eigenvalues
K <- matrix(c(-0.9,  0.3,
               0.6, -0.4), nrow = 2, byrow = TRUE)
ev <- eigen(K); round(ev$values, 4)           # two eigenvalues (negative for stability)
y0v <- c(10, 0)
cf  <- solve(ev$vectors, y0v)                 # coefficients from the initial condition
ysol <- function(t) ev$vectors %*% (exp(ev$values*t)*cf)
round(t(sapply(c(0, 1, 3, 8), function(s) drop(ysol(s)))), 4)

# Solve the same problem numerically and compare
library(deSolve)
num <- lsoda(y0v, c(0, 1, 3, 8), function(t, y, p) list(K %*% y), NULL)
round(num[, 2:3], 4)
c(max.abs.diff = max(abs(num[, 2:3] -
    t(sapply(c(0, 1, 3, 8), function(s) drop(ysol(s)))))))

# A single state is a sum of two exponentials: y1(t) = a1 exp(l1 t) + a2 exp(l2 t)
round(c(a1 = ev$vectors[1, 1]*cf[1], l1 = ev$values[1],
        a2 = ev$vectors[1, 2]*cf[2], l2 = ev$values[2]), 4)
