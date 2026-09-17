# 패키지가 대신 해 주던 계산을 직접 해 본다. TOST 의 검정력은 두 단측
# 검정을 모두 통과할 확률이고, 각 검정통계량은 비심 t 분포를 따른다.
# 비심도는 (동등한계 - 참 로그차)/SE 이고 SE = sqrt(MSE/n), n 은 순서군당이다.
pwrNC <- function(n, CV, GMR = 1, alpha = 0.05, lo = 0.80, hi = 1.25) {
  se <- sqrt(log(1 + CV^2)/n); df <- 2*(n - 1)
  T0 <- qt(1 - alpha, df);     d  <- log(GMR)
  pt(-T0, df, ncp = (log(lo) - d)/se) - pt(T0, df, ncp = (log(hi) - d)/se)
}
ssNC <- function(CV, GMR = 1, power = 0.8) {
  n <- 2; while (pwrNC(n, CV, GMR) <= power) n <- n + 1; n }

# 참 비가 정확히 1 이라고 볼 때의 표본크기 표 (순서군당 인원)
cvv <- seq(0.20, 0.40, 0.02)
round(t(sapply(cvv, function(v) { k <- ssNC(v)
  c(CV = v, MSE = log(1 + v^2), N.per.seq = k, power = pwrNC(k, v),
    T0 = qt(0.95, 2*(k - 1))) })), 5)

# 참 비가 1 이 아니면 비심도의 분자가 그만큼 옮겨간다. 이 장이 쓰는
# GMR 0.95, CV 25%, 총 28명에서 손 계산과 패키지를 맞춰 본다.
signif(c(hand.nct        = pwrNC(14, 0.25, 0.95),
         PowerTOST.nct   = power.TOST(CV = 0.25, theta0 = 0.95, n = 28,
                                      method = "nct"),
         PowerTOST.exact = power.TOST(CV = 0.25, theta0 = 0.95, n = 28),
         shifted.central = power.TOST(CV = 0.25, theta0 = 0.95, n = 28,
                                      method = "shifted")), 8)

# 근사가 어긋나는 자리는 표본이 작고 변동이 클 때다
round(t(sapply(c(8, 12, 20, 40), function(N)
  c(n = N, nct = pwrNC(N/2, 0.40, 0.95),
    exact = power.TOST(CV = 0.40, theta0 = 0.95, n = N),
    shifted = power.TOST(CV = 0.40, theta0 = 0.95, n = N,
                         method = "shifted")))), 5)
