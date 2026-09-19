# 가능도: 파라미터를 넣으면 '이 자료가 나올 법한 정도'를 돌려주는 함수
y <- log(x)                                   # 로그척도에서 다루면 정규다
m2ll <- function(p) -2*sum(dnorm(y, p[1], p[2], log = TRUE))
opt  <- optim(c(4, 0.5), m2ll, method = "L-BFGS-B",
              lower = c(0, 0.01), upper = c(10, 5))
round(c(mu.hat = opt$par[1], sd.hat = opt$par[2], m2LL = opt$value,
        mu.true = mu, sd.true = sg), 4)
# 최대가능도 추정치는 표본평균·표본표준편차(n 으로 나눈 것)와 같다
round(c(mean.y = mean(y), sd.n = sqrt(mean((y - mean(y))^2))), 4)

# 가능도의 모양을 본다: 뾰족하면 정밀하고 완만하면 아니다
mg <- seq(4.4, 4.75, 0.001)
pr <- sapply(mg, function(m) m2ll(c(m, opt$par[2])))
plot(mg, pr - min(pr), type = "l", las = 1, bty = "l", ylim = c(0, 12),
     xlab = expression(mu), ylab = expression(paste(-2, log, L, " (최솟값 기준)")))
abline(h = 3.84, lty = 2); abline(v = opt$par[1], lty = 3)
ci <- range(mg[pr - min(pr) <= 3.84])         # 가능도 기반 95% 신뢰구간
segments(ci[1], 0, ci[2], 0, lwd = 3); round(ci, 4)
