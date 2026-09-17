# 해석해와 수치해법. 2구획에 100 mg 을 일시주입하고 12시간 뒤를 본다.
# 기준값은 해석해이다. 중간 시각을 하나도 계산하지 않고 한 번에 간다.
ref12 <- as.vector(expAt(12) %*% c(100, 0))
round(c(중심구획 = ref12[1], 말초구획 = ref12[2]), 6)

# (a) 고정 걸음 4차 Runge-Kutta. 빠른 상의 반감기가 log(2)/lambda1 = 0.17시간이므로
#     걸음이 그보다 크면 발산한다(안정성 문제이지 정밀도 문제가 아니다).
rk4 <- function(h) {
  y <- c(100, 0)
  for (i in seq_len(round(12/h))) {
    k1 <- Amt %*% y;            k2 <- Amt %*% (y + h/2*k1)
    k3 <- Amt %*% (y + h/2*k2); k4 <- Amt %*% (y + h*k3)
    y  <- y + h/6*(k1 + 2*k2 + 2*k3 + k4)
  }
  as.vector(y)
}
hs <- c(2, 1, 0.5, 0.25, 0.1)
data.frame(h = hs, 걸음수 = 12/hs,
           상대오차 = signif(sapply(hs, function(h) rk4(h)[1]/ref12[1] - 1), 3))

# (b) 걸음을 스스로 정하는 lsoda. 허용오차를 풀면 그만큼 어긋난다.
tol <- 10^-c(2, 4, 6, 8, 10)
data.frame(tol = tol,
           상대오차 = signif(sapply(tol, function(x)
             ode(c(100, 0), c(0, 12), function(t, y, p) list(Amt %*% y),
                 NULL, rtol = x, atol = x)[2, 2]/ref12[1] - 1), 3))
