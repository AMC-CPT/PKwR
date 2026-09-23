# What do summary statistics estimate: one log-normal sample summarized three ways
set.seed(20260828)
x <- rlnorm(200, mu, sg)
gm  <- function(v) exp(mean(log(v)))
gcv <- function(v) sqrt(exp(var(log(v))) - 1)*100
round(c(arith.mean = mean(x), geo.mean = gm(x), median = median(x),
        SD = sd(x), gCV.pct = gcv(x)), 3)

# Compared with the true values, it becomes clear what estimates what
round(c(true.arith = exp(mu + sg^2/2), true.geo = exp(mu),
        true.median = exp(mu), true.CV.pct = 100*sqrt(exp(sg^2) - 1)), 3)
