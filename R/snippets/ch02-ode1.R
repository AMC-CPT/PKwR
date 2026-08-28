# 1계 선형 미분방정식 dy/dt = -k y 를 세 가지로 푼다
f  <- function(t, y) -k*y                     # 우변
y0 <- A; tend <- 20

euler <- function(h) {                        # 가장 단순한 수치해법
  tv <- seq(0, tend, h); y <- y0
  for (i in 1:(length(tv) - 1)) y <- y + h*f(tv[i], y)
  y
}
rk4 <- function(h) {                          # 4차 Runge-Kutta
  tv <- seq(0, tend, h); y <- y0
  for (i in 1:(length(tv) - 1)) {
    k1 <- f(tv[i], y);         k2 <- f(tv[i] + h/2, y + h/2*k1)
    k3 <- f(tv[i] + h/2, y + h/2*k2); k4 <- f(tv[i] + h, y + h*k3)
    y <- y + h/6*(k1 + 2*k2 + 2*k3 + k4)
  }
  y
}
exact <- A*exp(-k*tend)
hs <- c(2, 1, 0.5, 0.25)
err <- rbind(h = hs, Euler = sapply(hs, function(h) abs(euler(h) - exact)),
             RK4 = sapply(hs, function(h) abs(rk4(h) - exact)))
signif(err, 3)
# 단계를 절반으로 줄일 때 오차가 몇 배로 주는가 (차수의 정의)
round(rbind(Euler = err[2, -4]/err[2, -1], RK4 = err[3, -4]/err[3, -1]), 1)
