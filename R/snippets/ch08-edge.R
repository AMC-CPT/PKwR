# 분모가 0 이 되는 자리. 흡수속도상수가 제거속도상수에 가까워질 때이다.
# 식은 0/0 이 되고 그 극한은 Xg0*K*t*exp(-K*t) 이다 (식 (eq:lap1clim)).
oral1 <- function(Xg0, Ke, Ka, t) Ka*Xg0/(Ka - Ke)*(exp(-Ke*t) - exp(-Ka*t))
lim1  <- function(Xg0, K,  t)     Xg0*K*t*exp(-K*t)

dd <- 10^-c(2, 4, 6, 8, 10, 12, 14, 16)
data.frame(delta = dd,
           식 = sapply(dd, function(x) oral1(100, 0.1, 0.1 + x, 4)),
           극한식 = lim1(100, 0.1, 4),
           상대오차 = signif(sapply(dd, function(x)
             oral1(100, 0.1, 0.1 + x, 4)/lim1(100, 0.1, 4) - 1), 3))

# 2구획의 두 근은 언제나 서로 다른 실수이다(정리). 판별식이
# (K10 - K21)^2 + K12^2 + 2*K12*(K10 + K21) 와 같기 때문이다.
disc <- function(p) (p[1] - p[3])^2 + p[2]^2 + 2*p[2]*(p[1] + p[3])
set.seed(8)
pr <- matrix(10^runif(30000, -3, 1), ncol = 3)     # 속도상수를 0.001~10 에서 무작위 추출
c(항등식.최대오차 = max(abs(apply(pr, 1, function(x) disc(x) - (sum(x)^2 - 4*x[1]*x[3])))),
  최소.판별식     = min(apply(pr, 1, disc)),
  두.근.최소격차  = min(sqrt(apply(pr, 1, disc))))
