# Henderson-Hasselbalch: pH 에 따른 비이온형 분율
fnon.acid <- function(pH, pKa) 1/(1 + 10^(pH - pKa))    # 약산
fnon.base <- function(pH, pKa) 1/(1 + 10^(pKa - pH))    # 약염기
pH <- seq(1, 9, 0.02)
plot(pH, fnon.acid(pH, 3.5), type = "l", las = 1, bty = "l", ylim = c(0, 1),
     xlab = "pH", ylab = "Nonionized fraction")
lines(pH, fnon.base(pH, 9.0), lty = 2)
abline(v = c(1.5, 6.5, 7.4), lty = 3)
text(c(1.5, 6.5, 7.4), 1.04, c("stomach", "gut", "plasma"), cex = 0.75, xpd = TRUE)
legend("right", bty = "n", cex = 0.85, lty = 1:2,
       legend = c("weak acid (pKa 3.5)", "weak base (pKa 9.0)"))

# 이온 트래핑: 비이온형만 막을 지나 평형이 되면 총농도는 이온화가 큰 쪽에 쌓인다
ratio.base <- function(pH1, pH2, pKa) (1 + 10^(pKa - pH1))/(1 + 10^(pKa - pH2))
round(c(milk.vs.plasma  = ratio.base(7.0, 7.4, 8.0),     # 유즙 pH 7.0
        acidic.urine    = ratio.base(5.0, 7.4, 9.0)), 1) # 산성뇨 pH 5
