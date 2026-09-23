# Forest plot: covariate effect as a 'ratio of exposure' (standard regulatory figure).
# SE of theta4 via the LRT: dOFV ~ (theta/SE)^2, hence SE ~ |theta|/sqrt(dOFV)
th4 <- r.wt[["Final Estimates"]][4]
se4 <- abs(th4)/sqrt(dOFV)
round(c(theta4 = th4, dOFV = dOFV, SE.approx = se4,
        CI.lo = th4 - 1.96*se4, CI.hi = th4 + 1.96*se4), 3)

# CL = V*k and WT enters V only: exposure ratio AUC(WT)/AUC(70) = (WT/70)^(-theta4)
wt.q  <- round(quantile(unique(DATA[, c("ID", "BWT")])$BWT, c(.05, .25, .75, .95)), 1)
lr    <- -th4*log(wt.q/70)                         # log exposure ratio
se.lr <- se4*abs(log(wt.q/70))                     # delta method
fp <- cbind(ratio = exp(lr), lo = exp(lr - 1.96*se.lr), hi = exp(lr + 1.96*se.lr))
rownames(fp) <- paste0("WT ", wt.q, " kg"); round(fp, 3)

par(mar = c(4.2, 9.5, 1.6, 1.0))
plot(NA, xlim = c(0.6, 1.7), ylim = c(0.5, 4.5), yaxt = "n", las = 1, bty = "l",
     xlab = "AUC ratio vs 70 kg", ylab = "")
rect(0.8, 0.3, 1.25, 4.7, col = "gray92", border = NA); abline(v = 1, lty = 2)
segments(fp[, "lo"], 1:4, fp[, "hi"], 1:4); points(fp[, "ratio"], 1:4, pch = 16)
axis(2, at = 1:4, labels = rownames(fp), las = 1, tick = FALSE)
text(1.025, 4.45, "80-125%", cex = 0.7, adj = 0)
