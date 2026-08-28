t <- seq(0, 24, 0.05)
Conc <- Cpo(t, D = 100, V = 1, k = 0.15, ka = 1.0)   # 농도
Eff  <- 44*(exp(-0.08*t) - exp(-0.35*t))             # 효과(작용부위 지연 반영)

plot(t, Conc, type = "l", las = 1, bty = "l", ylim = c(0, 90),
     xlab = "Time", ylab = "Concentration or Effect")
lines(t, Eff, lty = 2)
text( 6, 66, "Concentration", pos = 4, cex = 0.85)
arrows(6, 64, 3.6, 54, length = 0.06)
text(14, 27, "Effect", pos = 4, cex = 0.85)
arrows(14, 26, 12, 20, length = 0.06)
