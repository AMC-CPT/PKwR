# Direct effect after a one-compartment IV bolus. Concentration falls on one half-life
# clock, but saturation puts the effect on another.  r0 = C0/EC50 (initial conc./EC50)
k  <- 0.3; EC50 <- 2; thalf <- log(2)/k           # elimination half-life 2.31 hr
r0 <- c(1, 4, 16, 64)
t  <- seq(0, 24, 0.005)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
matplot(t, sapply(r0, function(r) r*exp(-k*t)), type = "l", log = "y",
        lty = 1:4, col = 1, las = 1, bty = "l", ylim = c(0.1, 100), yaxt = "n",
        xlab = "Time (hr)", ylab = "C / EC50", main = "(a) concentration")
axis(2, at = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"), las = 1)
abline(h = 1, lty = 3); text(21, 1.4, "EC50", cex = 0.8)

matplot(t, sapply(r0, function(r) 100*r*exp(-k*t)/(1 + r*exp(-k*t))),
        type = "l", lty = 1:4, col = 1, las = 1, bty = "l", ylim = c(0, 100),
        xlab = "Time (hr)", ylab = "Effect (% of Emax)", main = "(b) effect")
legend("topright", bty = "n", cex = 0.8, lty = 4:1, legend = paste0("r0 = ", rev(r0)))

# (1) Time for which the effect stays above 50% (C > EC50): it grows by one half-life
#     each time the dose doubles.
# (2) Time to fall to half the initial effect (in half-lives): log2(2 + r0)
t50 <- sapply(r0, function(r) {                   # numerical check of (2)
  E <- 100*r*exp(-k*t)/(1 + r*exp(-k*t))
  t[which.max(E <= E[1]/2)]
})
round(rbind(dur.over.50.thalf = log(r0)/k/thalf,
            t50E.thalf = t50/thalf, theory.log2 = log2(2 + r0)), 3)
