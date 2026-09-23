# Curves of the three models. With V = 1 L and F = 1, the amount is the concentration.
# Plotting needs a fine grid, but since the analytic solution reaches each grid point
# by 'one advance from the previous point', a finer grid does not change the accuracy.
tg  <- sort(unique(c(seq(0, 60, 0.05), 24, 27, 48)))
dhf <- data.frame(TIME = tg, AMT = NA, RATE = NA, CMT = NA)
dhf[dhf$TIME ==  0, 2:4] <- c(100,  0, 2)
dhf[dhf$TIME == 24, 2:4] <- c(150, 50, 2)
dhf[dhf$TIME == 48, 2:4] <- c(100,  0, 1)
dhf <- ExpandDH(dhf)

# One compartment is just the case n = 1: adj is 1 and there is one root, so C = 1.
cc1 <- runN(dhf, 0.1,  list(matrix(1)))[, 2]
cc2 <- runN(dhf, lam,  Co )[, 2]
cc3 <- runN(dhf, lam3, Co3)[, 2]
c(one.comp.rule = max(abs(runN(dh2, 0.1, list(matrix(1))) - M1)))

ii <- dhf$TIME > 0                        # at time 0 (pre-dose) all three curves are 0
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (lg in c("", "y")) {
  matplot(dhf$TIME[ii], cbind(cc1, cc2, cc3)[ii, ], type = "l", lty = c(1, 2, 3),
          col = 1, log = lg, las = 1, bty = "l", xlab = "Time (hr)",
          ylab = "Central conc. (mg/L)", ylim = if (lg == "") c(0, 145) else c(1, 300),
          main = if (lg == "") "(a) Linear" else "(b) Semilog", cex.main = 0.95)
  abline(v = c(0, 24, 48), lty = 3, col = "gray60")
  if (lg == "") legend("topleft", bty = "n", cex = 0.85, lty = 1:3,
                       legend = c("1-compartment", "2-compartment", "3-compartment"))
}
