# flip-flop: ka < k 이면 겉보기 최종반감기가 흡수속도상수로 결정된다.
t <- seq(0.05, 48, 0.05)
k <- log(2)/4                          # 제거반감기 4 hr
Cnorm <- Cpo(t, D = 100, V = 10, k = k, ka = 1.2)     # ka > k  (정상)
Cflip <- Cpo(t, D = 100, V = 10, k = k, ka = 0.0578)  # ka < k  (flip-flop)

plot(t, Cnorm, type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration")
lines(t, Cflip, lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("ka = 1.2 /hr   (fast absorption)",
                  "ka = 0.0578 /hr   (slow absorption, flip-flop)"))

# 마지막 구간의 기울기로 구한 겉보기 반감기
app.half <- function(C) {
  s <- t >= 36
  unname(log(2)/(-coef(lm(log(C[s]) ~ t[s]))[2]))
}
c(k.true = log(2)/k, ka.slow = log(2)/0.0578,
  apparent.normal = app.half(Cnorm), apparent.flipflop = app.half(Cflip))
