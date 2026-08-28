# 경구 1구획(Bateman): 흡수와 소실이 겹쳐 두 지수의 차가 된다
ka <- 0.9; ke <- 0.25; Dose <- 100; V <- 30
bat  <- function(t) Dose*ka/(V*(ka - ke))*(exp(-ke*t) - exp(-ka*t))
tmax <- log(ka/ke)/(ka - ke)                  # dC/dt = 0 을 풀면 나온다
round(c(tmax = tmax, Cmax = bat(tmax), AUCinf = Dose/(V*ke)), 4)

# 같은 문제를 연립 미분방정식으로 풀어 대조한다
library(deSolve)
dyn <- function(t, y, p) list(c(-ka*y[1], ka*y[1] - ke*y[2]))
tp  <- c(0, 1, tmax, 4, 12)
num <- lsoda(c(Dose, 0), tp, dyn, NULL)
round(cbind(t = tp, numeric = num[, 3]/V, analytic = bat(tp)), 5)
c(max.abs.diff = max(abs(num[, 3]/V - bat(tp))))
