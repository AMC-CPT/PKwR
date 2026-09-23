# Three interval estimates for a variance. lh is R's luteinizing hormone data (n = 48).
library(LBI)                                  # author's likelihood inference package
n1 <- length(lh); v1 <- var(lh); cl <- 0.95
ci  <- (n1 - 1)*v1/qchisq(c(0.5 + cl/2, 0.5 - cl/2), n1 - 1)   # usual chi-square CI
lb  <- LBCIvar(lh,   conf.level = cl)["var", c("LL", "UL")]    # likelihood-based CI
li  <- LInormVar(lh, conf.level = cl)["var", c("LL", "UL")]    # likelihood interval
w   <- rbind(CI = ci, LBCI = lb, LI = li)
round(cbind(w, width = w[, 2] - w[, 1]), 4)

# Why they differ: compare the heights of the two endpoints on the likelihood curve
LL <- function(v) -n1/2*(log(2*pi*v) + (n1 - 1)*v1/(n1*v))     # profile log-likelihood
h  <- c(LI.LL = LL(li[[1]]), LI.UL = LL(li[[2]]),
        CI.LL = LL(ci[1]),   CI.UL = LL(ci[2])) - LL(v1)
round(h, 4)

vg <- seq(0.15, 0.60, 0.001)
plot(vg, LL(vg) - LL(v1), type = "l", las = 1, bty = "l", ylim = c(-5, 0.4),
     xlab = "Variance", ylab = "Log-likelihood (relative to maximum)")
abline(v = v1, lty = 3); abline(h = h[["LI.LL"]], lty = 3, col = "gray55")
segments(li, -5, li, h[1:2])                                   # two ends of the LI
segments(ci, -5, ci, h[3:4], lty = 2)                          # two ends of the CI
legend(0.33, -3.0, bty = "n", cex = 0.8, lty = c(1, 2),
       legend = c("LI: both ends at the same height", "CI: heights differ"))
