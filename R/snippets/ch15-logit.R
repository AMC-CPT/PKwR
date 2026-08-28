# 이분형 반응(있다/없다)의 농도-반응: 로지스틱 회귀가 sigmoid Emax 와 같은 모형이다.
set.seed(20260828)
EC50t <- 8; gmt <- 1.6                             # 참값
Cd15 <- rep(c(0.5, 1, 2, 4, 8, 16, 32, 64), each = 40)
pr   <- Cd15^gmt/(EC50t^gmt + Cd15^gmt)            # sigmoid Emax 를 확률로
Yb   <- rbinom(length(Cd15), 1, pr)

# logit(p) = b0 + b1 log C 는 p = C^b1/(exp(-b0/b1)^b1 + C^b1) 과 같은 식이다
gl <- glm(Yb ~ log(Cd15), family = binomial)
b  <- coef(gl)
round(c(b0 = b[[1]], b1 = b[[2]], gamma.hat = b[[2]],
        EC50.hat = exp(-b[[1]]/b[[2]]), gamma.true = gmt, EC50.true = EC50t), 3)

par(mar = c(4.2, 4.8, 1.0, 1.0))
obs <- tapply(Yb, Cd15, mean)                      # 농도별 관측 반응률
plot(as.numeric(names(obs)), obs, log = "x", las = 1, bty = "l", pch = 16,
     ylim = c(0, 1), xlab = "Concentration (mg/L)", ylab = "반응 확률")
cg <- 10^seq(-0.5, 2, 0.01)
lines(cg, plogis(b[[1]] + b[[2]]*log(cg)))
lines(cg, cg^gmt/(EC50t^gmt + cg^gmt), lty = 3)
abline(h = 0.5, lty = 3, col = "gray60")
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 3), pch = c(NA, NA),
       legend = c("로지스틱 적합", "참 sigmoid Emax"))
