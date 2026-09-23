# Loading dose: raise the first dose instead of waiting for steady state.
# The maintenance dose is multiplied by the accumulation factor.
V.map <- TH[[2]]*exp(fit5[2]); K.map <- TH[[3]]*exp(fit5[3])
DL <- D.new/(1 - exp(-K.map*12))                    # based on the accumulation factor
round(c(V.map = V.map, t.half = log(2)/K.map, accum = 1/(1 - exp(-K.map*12)),
        D.maint = D.new, D.load = DL, DL.by.CtV = 10*V.map), 1)

# Repeated dosing sums single-dose curves shifted in time (superposition, Chapter 5)
acc <- function(eta, t, Dm, tau, n, DL = NA) {
  out <- numeric(length(t))
  for (j in 0:(n - 1)) {
    dj <- if (j == 0 && !is.na(DL)) DL else Dm
    ok <- t >= j*tau
    out[ok] <- out[ok] + ipred(eta, t[ok] - j*tau, dj) }
  out }
tg2 <- seq(0, 72, 0.05)
plot(tg2, acc(eta.true, tg2, D.new, 12, 6), type = "l", lty = 2, las = 1,
     bty = "l", ylim = c(0, 24), xlab = "Time (hr)", ylab = "C (mg/L)",
     panel.first = { polygon(c(0, 72, 72, 0), c(5, 5, 15, 15), col = "gray92",
                             border = NA); abline(h = 10, lty = 3) })
lines(tg2, acc(eta.true, tg2, D.new, 12, 6, DL = DL))
legend("bottomright", bty = "n", cex = 0.85, lty = c(2, 1),
       legend = c("maintenance only", sprintf("loading %.0f mg + maintenance", DL)))
round(c(Cav.1st.noload = mean(acc(eta.true, seq(0, 12, 0.05), D.new, 12, 6)),
        Cav.1st.load = mean(acc(eta.true, seq(0, 12, 0.05), D.new, 12, 6, DL)),
        Cav.ss = mean(css(eta.true, seq(0, 12, 0.05), D.new, 12))), 2)
