# 비임상 독성시험의 희소 채혈: 시점마다 다른 동물을 희생하므로 개체별
# 곡선이 없다. 개체별 NCA 도 없고 군 평균 곡선 하나만 남는다.
set.seed(20260827)
tsp  <- c(0.5, 1, 2, 4, 8, 24); k <- length(tsp)            # 시점 6개
nsp  <- 4                                                   # 시점당 동물 4마리
mu   <- C2iv(tsp)                                           # 참 평균 곡선
Y    <- sapply(mu, function(m) m*exp(rnorm(nsp, 0, 0.30)))  # 개체간 CV 30%
ybar <- colMeans(Y); s2 <- apply(Y, 2, var)

# 사다리꼴은 평균들의 일차 결합이다. 그 계수 w 를 꺼내면 분산이 따라 나온다
w <- c((tsp[2] - tsp[1])/2, (tsp[3:k] - tsp[1:(k - 2)])/2, (tsp[k] - tsp[k - 1])/2)
AUC.b <- sum(w*ybar); SE.b <- sqrt(sum(w^2*s2/nsp))         # Bailer (1988)
round(c(w = w), 4)                                          # 사다리꼴의 계수
round(c(AUC.trap = auc.lin(tsp, ybar), AUC.w = AUC.b, SE = SE.b), 4)

# 검산: 시점 안에서 동물을 복원추출하는 붓스트랩과 맞는가
B  <- 2000
ab <- replicate(B, sum(w*apply(Y, 2, function(z) mean(sample(z, nsp, TRUE)))))
round(c(SE.bailer = SE.b, SE.boot = sd(ab),
        SE.boot.adj = sd(ab)*sqrt(nsp/(nsp - 1))), 4)       # n=4 의 (n-1)/n 편향 보정

round(c(lo.normal = AUC.b - 1.96*SE.b, hi.normal = AUC.b + 1.96*SE.b,
        lo.boot = quantile(ab, 0.025), hi.boot = quantile(ab, 0.975)), 4)

# 분산이 생겼으니 검정을 할 수 있다: 2배 용량이 2배 노출인가
Y2 <- sapply(2*mu, function(m) m*exp(rnorm(nsp, 0, 0.30)))
A2 <- sum(w*colMeans(Y2)); S2 <- sqrt(sum(w^2*apply(Y2, 2, var)/nsp))
dn <- A2/2 - AUC.b                                          # 용량보정 차이
se <- sqrt((S2/2)^2 + SE.b^2)
round(c(AUC.low = AUC.b, AUC.high = A2, ratio = A2/AUC.b,
        diff.dn = dn, se.dn = se, z = dn/se, p = 2*pnorm(-abs(dn/se))), 4)
