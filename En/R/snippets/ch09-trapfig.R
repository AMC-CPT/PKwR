# How the linear trapezoid overestimates a declining interval under sparse sampling
t.sp <- c(8, 24, 48)                                   # sparse terminal samples
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
tt <- seq(8, 48, 0.1)
plot(tt, C2iv(tt), type = "l", las = 1, bty = "l", ylim = c(0, 1.8),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)", main = "(a) linear")
polygon(c(t.sp, rev(t.sp)), c(C2iv(t.sp), 0, 0, 0), col = "gray88",
        border = "gray55")
lines(tt, C2iv(tt), lwd = 1.2); points(t.sp, C2iv(t.sp), pch = 16)

plot(tt, C2iv(tt), type = "l", las = 1, bty = "l", ylim = c(0, 1.8),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)", main = "(b) log")
i1 <- tt <= 24
polygon(c(tt[i1], 24, 8), c(C2iv(8)*exp(-log(C2iv(8)/C2iv(24))/16*(tt[i1] - 8)),
        0, 0), col = "gray88", border = "gray55")
i2 <- tt >= 24
polygon(c(tt[i2], 48, 24), c(C2iv(24)*exp(-log(C2iv(24)/C2iv(48))/24*(tt[i2] - 24)),
        0, 0), col = "gray88", border = "gray55")
lines(tt, C2iv(tt), lwd = 1.2); points(t.sp, C2iv(t.sp), pch = 16)
