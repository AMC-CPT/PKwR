# 두 오류와 검정력: 귀무분포와 대립분포가 겹치는 만큼 오류가 생긴다
n3 <- 16; se <- 1/sqrt(n3); d <- 0.8          # 참 차이 0.8, 표준오차 0.25
cv <- qnorm(0.975)*se                         # 양측 5% 임계값
round(c(SE = se, crit = cv, power = 1 - pnorm(cv, d, se) + pnorm(-cv, d, se)), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
xg <- seq(-1.2, 2.2, 0.005)
plot(xg, dnorm(xg, 0, se), type = "l", las = 1, bty = "l", ylim = c(0, 1.75),
     xlab = "관측된 차이", ylab = "밀도", main = "(a) 두 분포의 겹침")
lines(xg, dnorm(xg, d, se), lty = 2)
polygon(c(xg[xg > cv], rev(xg[xg > cv])), c(dnorm(xg[xg > cv], 0, se),
        rep(0, sum(xg > cv))), col = "gray80", border = NA)     # 1종 오류
polygon(c(xg[xg < cv], rev(xg[xg < cv])), c(dnorm(xg[xg < cv], d, se),
        rep(0, sum(xg < cv))), density = 12, angle = 45, border = NA)  # 2종
abline(v = cv, lty = 3)
legend("topright", bty = "n", cex = 0.75, lty = c(1, 2),
       legend = c("귀무(차이 0)", "대립(차이 0.8)"))

ng <- 4:40                                    # 표본수에 따른 검정력 곡선
pw <- sapply(ng, function(k) { s <- 1/sqrt(k); c <- qnorm(0.975)*s
                               1 - pnorm(c, d, s) + pnorm(-c, d, s) })
plot(ng, pw, type = "l", las = 1, bty = "l", ylim = c(0, 1),
     xlab = "군당 표본수", ylab = "검정력", main = "(b) 검정력 곡선")
abline(h = 0.8, lty = 2); abline(v = ng[which.max(pw >= 0.8)], lty = 3)
c(n.for.80pct = ng[which.max(pw >= 0.8)])
