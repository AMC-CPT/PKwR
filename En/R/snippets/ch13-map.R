# MAP objective: the EBE formula of Chapter 11 unchanged. The variance is evaluated
# at the current eta's prediction (INTERACTION). Two parts: data + penalty (prior).
iOM <- solve(OM)
mapObj <- function(eta, t, y, D) {
  f <- ipred(eta, t, D)
  v <- f^2*SG[["prop"]] + SG[["add"]]
  sum(log(v) + (y - f)^2/v) + drop(t(eta) %*% iOM %*% eta)
}
map <- function(i)                                  # i: indices of the samples to use
  optim(c(0, 0, 0), mapObj, t = t.pt[i], y = dv.pt[i], D = D,
        method = "BFGS")$par

fit1 <- map(5)                                      # one point at 12 h
fit3 <- map(c(2, 4, 5))                             # three points: 2, 8, 12 h
fit5 <- map(1:5)                                    # all five points
ind <- function(eta)
  c(KA = TH[[1]]*exp(eta[1]), V = TH[[2]]*exp(eta[2]),
    CL = TH[[2]]*exp(eta[2])*TH[[3]]*exp(eta[3]))
round(rbind(prior = ind(c(0, 0, 0)), map.1pt = ind(fit1),
            map.3pt = ind(fit3), map.5pt = ind(fit5), true = ind(eta.true)), 3)

tg <- seq(0.05, 14, 0.05)
plot(t.pt, dv.pt, las = 1, bty = "l", pch = 16, xlim = c(0, 14),
     ylim = c(0, 10), xlab = "Time (hr)", ylab = "Concentration (mg/L)")
lines(tg, ipred(c(0, 0, 0), tg, D), lty = 3)
lines(tg, ipred(fit1, tg, D), lty = 2)
lines(tg, ipred(fit5, tg, D), lty = 1)
lines(tg, ipred(eta.true, tg, D), col = "gray70", lwd = 2.5)
legend("topright", bty = "n", cex = 0.85,
       lty = c(3, 2, 1, 1), lwd = c(1, 1, 1, 2.5),
       col = c(1, 1, 1, "gray70"),
       legend = c("prior (population)", "MAP, 1 sample", "MAP, 5 samples",
                  "true individual"))
