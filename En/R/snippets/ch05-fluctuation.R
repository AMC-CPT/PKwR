# Same daily dose (600 mg/day), different dosing intervals: the fluctuation differs.
k <- log(2)/12; ka <- 0.7
t <- seq(0, 120, 0.05)
Cmulti <- function(t, S, tau)
  S*(acc(t, k, tau)*exp(-k*tprm(t, tau)) - acc(t, ka, tau)*exp(-ka*tprm(t, tau)))

C24 <- Cmulti(t, S = 173.1, tau = 24)   # 600 mg q24h
C08 <- Cmulti(t, S =  57.7, tau =  8)   # 200 mg q8h

plot(t, C24, type = "l", lty = 2, las = 1, bty = "l", ylim = c(0, 250),
     xlab = "Time (hr)", ylab = "Concentration")
lines(t, C08)
legend("topright", bty = "n", cex = 0.85, lty = c(2, 1),
       legend = c("600 mg q 24 hr", "200 mg q 8 hr"))

# Peak and trough concentrations and the fluctuation over the last dosing interval
fluct <- function(C, t, tau) {
  s <- t >= (120 - tau)
  c(Cmax = max(C[s]), Cmin = min(C[s]),
    fluctuation = (max(C[s]) - min(C[s]))/mean(C[s]))
}
rbind(q24h = fluct(C24, t, 24), q8h = fluct(C08, t, 8))
