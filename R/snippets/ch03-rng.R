# 균등난수는 사실 결정적 점화식이다 (선형합동법). 시드가 수열 전체를 정한다.
lcg <- function(n, seed = 7) { v <- numeric(n + 1); v[1] <- seed
  for (i in 1:n) v[i + 1] <- (16807*v[i]) %% 2147483647
  v[-1]/2147483647 }
u0 <- lcg(5000)
round(c(mean = mean(u0), SD = sd(u0), SD.theory = sqrt(1/12),
        lag1.cor = cor(u0[-1], u0[-length(u0)])), 4)

# 역변환법: F 의 역함수에 균등난수를 넣으면 F 를 따르는 난수가 된다
set.seed(20260828)
u1 <- runif(5000); ex <- -log(1 - u1)/0.25    # rate 0.25 인 지수분포
round(c(mean.inverse = mean(ex), mean.true = 1/0.25,
        ks.p = ks.test(ex, "pexp", 0.25)$p.value), 4)
