# 대사체: 생성은 모약물의 양이, 제거는 대사체 자신의 양이 구동한다.
# 수학적으로 경구 흡수의 Bateman 식과 같은 꼴이다 (생성 <-> 흡수).
kp <- 0.35; Vp <- 20; Dp <- 200; fm <- 0.6; Vm <- 20   # 모약물 t1/2 = 2 hr
Cp.t <- function(t) Dp/Vp*exp(-kp*t)
Cm.t <- function(t, km)
  fm*kp*Dp/(Vm*(km - kp))*(exp(-kp*t) - exp(-km*t))

km.f <- 4*kp; km.e <- kp/4               # 생성속도 제한 / 제거속도 제한
t <- seq(0.05, 48, 0.05)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(t, Cp.t(t), type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)",
     main = "(a) formation-limited (km > kp)")
lines(t, Cm.t(t, km.f), lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("parent", "metabolite"))

plot(t, Cp.t(t), type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)",
     main = "(b) elimination-limited (km < kp)")
lines(t, Cm.t(t, km.e), lty = 2)

# 대사체의 말기 기울기: (a)에서는 모약물의 kp 를, (b)에서는 자신의 km 을 따른다
slope <- function(km, t1, t2) { tt <- seq(t1, t2, 0.5)
  -coef(lm(log(Cm.t(tt, km)) ~ tt))[[2]] }
round(c(kp = kp, slope.a = slope(km.f, 12, 24),
        km.e = km.e, slope.b = slope(km.e, 36, 48)), 4)

# AUC 비의 해석해 AUCm/AUCp = fm CLp/CLm 을 수치적분으로 확인 ((a)의 경우)
round(c(formula = fm*(kp*Vp)/(km.f*Vm),
        numeric = integrate(Cm.t, 0, Inf, km = km.f)$value /
                  integrate(Cp.t, 0, Inf)$value), 4)
