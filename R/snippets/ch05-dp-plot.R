par(mfrow = c(1, 2), mar = c(4.2, 5.6, 2.4, 0.8))

# (a) log-log 산점도와 검정력 모형 적합선, 그리고 기울기 1 의 기준선
plot(AUC ~ Dose, data = pk, log = "xy", las = 1, bty = "l", pch = 1, cex = 0.7,
     xlab = "Dose (mg)", ylab = "", main = "(a) power model")
title(ylab = "AUClast (ug*h/L)", line = 4.2)
D.grid <- exp(seq(log(90), log(1000), 0.02))
lines(D.grid, exp(fixef(fit)[1] + fixef(fit)[2]*log(D.grid)))
lines(D.grid, exp(fixef(fit)[1] +               log(D.grid)), lty = 2)
legend("bottomright", bty = "n", cex = 0.8, lty = c(1, 2),
       legend = c(sprintf("fitted slope = %.3f", fixef(fit)[2]), "slope = 1"))

# (b) 용량 정규화 AUC: 용량에 따른 경향이 없어야 선형이다.
boxplot(AUC.dn ~ Dose, data = pk, las = 1, bty = "l", xlab = "Dose (mg)",
        ylab = "", main = "(b) dose-normalized")
title(ylab = "AUClast / Dose", line = 4.2)
