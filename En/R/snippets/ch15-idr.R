# Indirect response, kin inhibited: dR/dt = kin (1 - Cp/(IC50 + Cp)) - kout R (Imax = 1)
# Solved by implementing 4th-order Runge-Kutta, the general method for nonlinear ODEs
rk4 <- function(f, y0, t) {
  y <- numeric(length(t)); y[1] <- y0
  for (i in 1:(length(t) - 1)) {
    h  <- t[i + 1] - t[i]
    k1 <- f(t[i],       y[i])
    k2 <- f(t[i] + h/2, y[i] + h/2*k1)
    k3 <- f(t[i] + h/2, y[i] + h/2*k2)
    k4 <- f(t[i] + h,   y[i] + h*k3)
    y[i + 1] <- y[i] + h/6*(k1 + 2*k2 + 2*k3 + k4)
  }
  y
}

kin <- 8; kout <- 0.08; IC50 <- 1; V <- 20; k <- 0.15
didr <- function(D) function(t, R) {                  # derivative for each dose
  Cp <- D/V*exp(-k*t)
  kin*(1 - Cp/(IC50 + Cp)) - kout*R
}
t  <- seq(0, 96, 0.05)
R1 <- rk4(didr(100), 100, t)                          # baseline R0 = kin/kout = 100
R4 <- rk4(didr(400), 100, t)
round(c(t.nadir.100 = t[which.min(R1)], t.nadir.400 = t[which.min(R4)],
        R.min.100 = min(R1), R.min.400 = min(R4)), 1)

plot(t, R1, type = "l", las = 1, bty = "l", ylim = c(0, 105),
     xlab = "Time (hr)", ylab = "Response")
lines(t, R4, lty = 2)
abline(h = 100, lty = 3)
legend("bottomright", bty = "n", cex = 0.85, lty = c(1, 2, 3),
       legend = c("Dose 100", "Dose 400", "baseline"))
