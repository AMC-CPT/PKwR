# 최소제곱의 닫힌 해: b = (X'X)^-1 X'y. 9장의 말기 기울기 회귀가 이 계산이다.
tt3 <- c(6, 8, 12, 16, 24); ct <- c(9.1, 6.3, 3.1, 1.5, 0.34)
X <- cbind(Intercept = 1, TIME = tt3); yv <- log(ct)
b <- drop(solve(t(X) %*% X) %*% t(X) %*% yv)
round(c(b, lambda.z = -b[[2]], half.life = log(2)/-b[[2]]), 4)

# 표준오차는 잔차분산과 (X'X)^-1 의 대각에서 나온다
n5 <- length(yv); p5 <- ncol(X)
e5 <- drop(yv - X %*% b); mse <- sum(e5^2)/(n5 - p5)
se <- sqrt(diag(mse*solve(t(X) %*% X)))
r5 <- summary(lm(yv ~ tt3))
round(rbind(by.hand = c(b, se), lm = c(coef(r5)[, 1], coef(r5)[, 2])), 5)
round(c(R2.by.hand = 1 - sum(e5^2)/sum((yv - mean(yv))^2), R2.lm = r5$r.squared), 5)
