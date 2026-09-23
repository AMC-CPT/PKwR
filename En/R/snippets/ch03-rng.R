# Uniform deviates are a deterministic recurrence (LCG): the seed fixes the sequence
lcg <- function(n, seed = 7) { v <- numeric(n + 1); v[1] <- seed
  for (i in 1:n) v[i + 1] <- (16807*v[i]) %% 2147483647
  v[-1]/2147483647 }
u0 <- lcg(5000)
round(c(mean = mean(u0), SD = sd(u0), SD.theory = sqrt(1/12),
        lag1.cor = cor(u0[-1], u0[-length(u0)])), 4)

# Inverse transform: a uniform deviate fed into the inverse of F follows F
set.seed(20260828)
u1 <- runif(5000); ex <- -log(1 - u1)/0.25    # exponential with rate 0.25
round(c(mean.inverse = mean(ex), mean.true = 1/0.25,
        ks.p = ks.test(ex, "pexp", 0.25)$p.value), 4)
