# Initial estimates are read from the data: intercept and slope of the semilog line
lf <- lm(log(DV) ~ x, d4)
ie <- c(A = exp(coef(lf)[[1]]), k = -coef(lf)[[2]])
round(c(ie, SSE = sse(ie)), 4)

# This problem is insensitive to starting values: all 9 starts reach the same answer
starts <- expand.grid(A = c(5, 50, 500), k = c(0.02, 0.2, 2))
sol <- t(apply(starts, 1, function(s)
  optim(as.numeric(s), sse, method = "L-BFGS-B",
        lower = c(1e-3, 1e-3), upper = c(1e4, 20))$par))
c(n.start = nrow(sol), n.distinct = nrow(unique(round(sol, 2))))
round(range(sol[, 2]), 4)                     # range of the k estimates
