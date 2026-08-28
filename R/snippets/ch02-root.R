# 근 찾기: f(x) = 0 인 x. uniroot 는 부호가 바뀌는 구간 안에서 안전하게 찾는다.
Cinf <- function(t, R0 = 50, CL = 5, Vd = 30) R0/CL*(1 - exp(-CL/Vd*t))
f90  <- function(t) Cinf(t) - 0.9*50/5        # 항정상태의 90% 에 이르는 시각
u <- uniroot(f90, c(0, 100), tol = 1e-10)
round(c(uniroot = u$root, exact = -log(0.1)*30/5, iter = u$iter), 4)

# Newton-Raphson: 도함수를 알면 훨씬 빨리 수렴한다 (경구 모형의 tmax)
gg  <- function(t) ka*exp(-ka*t) - ke*exp(-ke*t)          # dC/dt = 0
ggp <- function(t) -ka^2*exp(-ka*t) + ke^2*exp(-ke*t)
tn <- 1; path <- tn
for (i in 1:5) { tn <- tn - gg(tn)/ggp(tn); path <- c(path, tn) }
signif(path - log(ka/ke)/(ka - ke), 3)        # 오차: 반복마다 자릿수가 배로 는다
c(newton = tn, closed.form = log(ka/ke)/(ka - ke))
