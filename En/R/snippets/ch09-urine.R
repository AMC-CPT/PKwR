# NCA of urine data: renal clearance and fe from the amount excreted Ae per interval
# Urine data are generated from the true model with fe = 0.4 (CLr = 1.8 L/hr).
fe.true <- 0.4; CLr.true <- fe.true*CL
tU  <- c(0, 4, 8, 12, 24, 48)                       # collection interval bounds (hr)
AeI <- sapply(2:length(tU), function(i)             # true interval excretion (mg)
         CLr.true*integrate(C2iv, tU[i - 1], tU[i])$value)
set.seed(20260827)                                  # 5% measurement error (log-normal)
AeI <- AeI*exp(rnorm(length(AeI), 0, 0.05))
Ae  <- cumsum(AeI)
round(rbind(t.end = tU[-1], Ae.interval = AeI, Ae.cum = Ae), 2)

# Renal clearance: divide by the plasma AUC over the same period (0-48 h)
CLr <- Ae[length(Ae)]/AUC.lst
# fe: the naive Ae/D misses the tail beyond 48 hours.
#     Correct it as fe = CLr/CL = (Ae/D) x (AUCinf/AUClast).
round(c(CLr = CLr, fe.naive = Ae[length(Ae)]/D,
        fe = Ae[length(Ae)]/D*AUC.inf/AUC.lst, fe.true = fe.true), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
plot(tU[-1], Ae, las = 1, bty = "l", pch = 16, xlim = c(0, 48),
     ylim = c(0, 115), xlab = "Time (hr)", ylab = "Cumulative Ae (mg)",
     main = "(a) cumulative excretion")
lines(tU[-1], Ae, lty = 1)
abline(h = fe.true*D, lty = 3)
text(1, fe.true*D + 7, "fe x Dose", cex = 0.8, pos = 4)

mid  <- (head(tU, -1) + tU[-1])/2                   # interval midpoints
rate <- AeI/diff(tU)                                # excretion rate (mg/hr)
plot(C2iv(mid), rate, las = 1, bty = "l", pch = 16, log = "xy",
     xlab = "C at interval midpoint (mg/L)", ylab = "Excretion rate (mg/hr)",
     main = "(b) rate vs concentration")
abline(a = log10(CLr.true), b = 1, lty = 3)         # slope = renal clearance
