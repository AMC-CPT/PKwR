# 반복투여 축적: n 번째 투여 직후의 축적 인자와 직전 투여 후 경과시간
acc  <- function(t, k, tau) (1 - exp(-(floor(t/tau)+1)*k*tau))/(1 - exp(-k*tau))
tprm <- function(t, tau) t - floor(t/tau)*tau

# 반감기 12 hr, 투여간격 12 hr
k <- log(2)/12; tau <- 12
t <- seq(0, 120, 0.05)

Cb <- 50 * acc(t, k, tau) * exp(-k*tprm(t, tau))              # IV bolus q12h
ka <- 0.25
Co <- 65.02*(acc(t, k, tau)*exp(-k*tprm(t, tau)) -
             acc(t, ka, tau)*exp(-ka*tprm(t, tau)))           # 경구 q12h
Ci <- 75*(1 - exp(-k*t))                                      # 지속정주

plot(t, Cb, type = "l", las = 1, bty = "l", ylim = c(0, 125),
     xlab = "Time (hr)", ylab = "Concentration")
lines(t, Co, lty = 2)
lines(t, Ci, lty = 3, lwd = 2)
legend("bottomright", bty = "n", cex = 0.85, lty = c(1, 2, 3), lwd = c(1, 1, 2),
       legend = c("IV bolus injection", "Oral administration",
                  "Continuous IV infusion"))
mtext("Half-life = 12 hr", side = 3, line = -1.4, adj = 0.97, cex = 0.85)
