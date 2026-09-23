# Simulated dose-proportionality study: 12 subjects x 3 doses, crossover, oral 1-cpt.
# Interindividual variability on CL and V is log-normal (CV 25%, 20%).
nsub  <- 12
doses <- c(100, 300, 900)
tobs  <- c(0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24)

set.seed(20260821)
CLi <- 5.0 * exp(rnorm(nsub, 0, 0.25))     # L/hr
Vi  <-  50 * exp(rnorm(nsub, 0, 0.20))     # L
kai <- 1.2 * exp(rnorm(nsub, 0, 0.30))     # /hr

dat <- do.call(rbind, lapply(seq_len(nsub), function(i)
  do.call(rbind, lapply(doses, function(D) data.frame(
    Subject = i, Dose = D, Time = tobs,
    conc = Cpo(tobs, D = D*1000, V = Vi[i], k = CLi[i]/Vi[i], ka = kai[i]) *
             exp(rnorm(length(tobs), 0, 0.08)))))))   # 8% measurement error

str(dat)
head(dat, 4)
