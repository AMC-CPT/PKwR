# VPC: simulate 500 virtual trials at the final estimates and overlay the observations.
# Reproducibility: seed set; unit = subject (eta); sampling design as in the raw data.
set.seed(20260827)
nsim <- 500
L <- t(chol(OM))                                   # eta ~ MVN(0, OM)
sim <- replicate(nsim, {
  do.call(c, lapply(unique(DATA$ID), function(id) {
    Di <- DATA[DATA$ID == id, ]
    Fi <- PRED(TH, as.numeric(L %*% rnorm(3)), Di)[, "F"]
    Fi*(1 + rnorm(length(Fi), 0, sqrt(SG[1, 1]))) +
      rnorm(length(Fi), 0, sqrt(SG[2, 2]))
  }))
})

tnom <- c(0, 0.25, 0.5, 1, 2, 3.5, 5, 7, 9, 12, 24)   # nominal sampling times
bin  <- tnom[apply(abs(outer(DATA$TIME, tnom, "-")), 1, which.min)]
qs   <- apply(sim, 1, quantile, c(0.05, 0.5, 0.95))    # predicted quantiles per point
band <- apply(qs, 1, tapply, bin, median)              # summarize by time bin

plot(DATA$TIME, DATA$DV, las = 1, bty = "l", pch = 1, cex = 0.7,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
polygon(c(tnom, rev(tnom)), c(band[, "5%"], rev(band[, "95%"])),
        col = "gray90", border = NA)
points(DATA$TIME, DATA$DV, pch = 1, cex = 0.7)
lines(tnom, band[, "50%"], lwd = 1.2)                  # predicted median
lines(tnom, tapply(DATA$DV, bin, median), lty = 2)     # observed median
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2, NA), pch = c(NA, NA, 22),
       pt.bg = "gray90", pt.cex = 1.8,
       legend = c("simulated median", "observed median", "simulated 90% PI"))
