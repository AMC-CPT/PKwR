# flip-flop: when ka < k, the apparent terminal half-life is determined by ka.
t <- seq(0.05, 48, 0.05)
k <- log(2)/4                          # elimination half-life 4 hr
Cnorm <- Cpo(t, D = 100, V = 10, k = k, ka = 1.2)     # ka > k  (normal)
Cflip <- Cpo(t, D = 100, V = 10, k = k, ka = 0.0578)  # ka < k  (flip-flop)

plot(t, Cnorm, type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration")
lines(t, Cflip, lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("ka = 1.2 /hr   (fast absorption)",
                  "ka = 0.0578 /hr   (slow absorption, flip-flop)"))

# apparent half-life from the slope of the last segment
app.half <- function(C) {
  s <- t >= 36
  unname(log(2)/(-coef(lm(log(C[s]) ~ t[s]))[2]))
}
c(k.true = log(2)/k, ka.slow = log(2)/0.0578,
  apparent.normal = app.half(Cnorm), apparent.flipflop = app.half(Cflip))
