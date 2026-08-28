# 이종간 스케일링: 청소율은 체중의 거듭제곱을 따른다.  CL = a * W^b
W  <- c(mouse = 0.02, rat = 0.25, rabbit = 2.5, monkey = 5, dog = 12)  # kg
CL <- c(0.099, 0.77, 3.7, 7.3, 12.2)                                   # L/hr

fit <- lm(log(CL) ~ log(W))
a <- exp(coef(fit)[[1]]); b <- coef(fit)[[2]]
CL.human <- a * 70^b                    # 70 kg 성인의 예측 청소율

# 동물에서 유효했던 노출(AUC 5 mg*hr/L)을 재현하는 1일 용량 = CL * AUC
round(c(a = a, b = b, CL.human = CL.human, Dose.mg = CL.human * 5), 3)

plot(W, CL, log = "xy", las = 1, bty = "l", pch = 16, xaxt = "n", yaxt = "n",
     xlim = c(0.01, 100), ylim = c(0.05, 60),
     xlab = "Body weight (kg)", ylab = "Clearance (L/hr)")
axis(1, at = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"))
axis(2, at = c(0.05, 0.5, 5, 50), las = 1)
curve(a * x^b, from = 0.01, to = 100, add = TRUE)
points(70, CL.human, pch = 1, cex = 1.3)
text(W, CL, names(W), pos = c(4, 4, 2, 4, 3), cex = 0.75)
text(70, CL.human, "human (predicted)", pos = 2, cex = 0.75)
