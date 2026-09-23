# If only one point can be measured, when? The posterior information answers.
# posterior info = prior info + data info = Om^-1 + G' Vy^-1 G  (as in Chapter 11)
postSD <- function(tt) {
  f  <- ipred(c(0, 0, 0), tt, D)                    # linearize at prior prediction
  G  <- jacobian(function(e) ipred(e, tt, D), c(0, 0, 0))
  Vy <- diag(f^2*SG[["prop"]] + SG[["add"]], length(tt))
  Vq <- solve(iOM + t(G) %*% solve(Vy) %*% G)
  c(KA = sqrt(Vq[1, 1]), V = sqrt(Vq[2, 2]),
    logCL = sqrt(sum(Vq[2:3, 2:3])))                # var(etaV + etaK)
}
tgd <- seq(0.25, 24, 0.25)
S <- t(sapply(tgd, postSD))
prior <- c(sqrt(OM[1, 1]), sqrt(OM[2, 2]), sqrt(sum(OM[2:3, 2:3])))
round(c(best.t.CL = tgd[which.min(S[, "logCL"])], SD.at.best = min(S[, "logCL"]),
        best.t.KA = tgd[which.min(S[, "KA"])],    prior.SD.logCL = prior[3]), 3)

matplot(tgd, t(t(S)/prior), type = "l", lty = c(2, 3, 1), col = 1, las = 1,
        bty = "l", ylim = c(0, 1.05), xlab = "Sampling time (hr)",
        ylab = "Posterior SD / prior SD")
abline(v = tgd[which.min(S[, "logCL"])], lty = 3, col = "gray55")
legend("bottomright", bty = "n", cex = 0.85, lty = c(2, 3, 1),
       legend = c("KA", "V", "log CL"))
