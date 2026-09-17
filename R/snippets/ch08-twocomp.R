# 2구획 전체 해 = 제차해 + 주입의 특수해 + 경구의 특수해 (식 (eq:lapall)).
# 근 lam 과 계수 Co 는 ch08-expat 에서 구한 것이다. 구획 수에 무관한 꼴이므로
# 3구획에서도 그대로 쓴다.
Ka <- 1
advN <- function(st, R, dt, L, Cf) {          # st = c(흡수부위, 구획 1..n)
  eL   <- exp(-L*dt)
  eA   <- Reduce(`+`, Map(function(C, e) C*e, Cf, eL))     # exp(A dt)
  col1 <- sapply(Cf, function(C) C[, 1])                   # 입력은 1번 구획으로 들어간다
  c(st[1]*exp(-Ka*dt),
    eA %*% st[-1]                               +          # 제차해
    R*(col1 %*% ((1 - eL)/L))                   +          # 주입
    Ka*st[1]*(col1 %*% ((eL - exp(-Ka*dt))/(Ka - L))))     # 경구
}
runN <- function(DH, L, Cf) {
  M  <- matrix(0, nrow(DH), length(L) + 1,
               dimnames = list(NULL, c("Xg", paste0("X", seq_along(L)))))
  st <- numeric(length(L) + 1)
  for (i in seq_len(nrow(DH))) {
    if (i > 1) st <- advN(st, DH$RATE2[i - 1], DH$TIME[i] - DH$TIME[i - 1], L, Cf)
    M[i, ] <- st                                           # 용량을 넣기 전 값
    if (DH$BOLUS[i] > 0) st[DH$CMT[i]] <- st[DH$CMT[i]] + DH$BOLUS[i]
  }
  M
}

M2 <- runN(dh2, lam, Co)
round(cbind(TIME = dh2$TIME, M2)[c(1, 2, 7, 8, 10, 13, 14, 15, 19), ], 4)

# wnl 의 nComp, 그리고 deSolve 의 수치적분과 대조한다(용량이 없는 시각에서).
library(deSolve)
dydt2 <- function(t, y, pr) list(c(-Ka*y[1],
                                   Ka*y[1] - (K10 + K12)*y[2] + K21*y[3] + pr$R(t),
                                   K12*y[2] - K21*y[3]))
evt <- data.frame(var = c("X1", "Xg"), time = c(0, 48), value = 100, method = "add")
num <- ode(c(Xg = 0, X1 = 0, X2 = 0), dh2$TIME, dydt2,
           list(R = approxfun(c(0, 24, 27, 60), c(0, 50, 0, 0),
                              method = "constant", rule = 2)),
           events = list(data = evt), rtol = 1e-10, atol = 1e-10)
nod <- dh2$BOLUS == 0
c(vs.nComp   = max(abs(M2 - nComp(SolComp2(K10, K12, K21), Ka = Ka, dh2))),
  vs.deSolve = max(abs(M2[nod, 2] - num[nod, 3])))
