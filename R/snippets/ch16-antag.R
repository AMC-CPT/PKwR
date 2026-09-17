# 경쟁적 길항: 겉보기 EC50 이 (1 + Cb/Kb) 배로 커진다 (같은 자리 경쟁)
# 비경쟁적 길항: EC50 은 그대로 두고 Emax 자체를 깎는다
CA    <- 10^seq(-1, 3, 0.005)                     # 작용제 농도 (로그 눈금)
ratio <- c(0, 1, 4, 9)                            # Cb/Kb: 길항제 농도/해리상수

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CA, Emax.model(CA, 100, 5), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 112), xlab = "Agonist concentration", ylab = "Effect",
     main = "(a) competitive")
for (r in ratio[-1]) lines(CA, Emax.model(CA, 100, 5*(1 + r)), lty = 2)
abline(h = 50, lty = 3)
text(c(5, 10, 25, 50), 58, labels = ratio, cex = 0.75)
text(0.25, 105, "Cb/Kb", cex = 0.75, pos = 4)

plot(CA, Emax.model(CA, 100, 5), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 112), xlab = "Agonist concentration", ylab = "Effect",
     main = "(b) noncompetitive")
for (f in c(0.75, 0.50, 0.25)) lines(CA, f*Emax.model(CA, 100, 5), lty = 2)
text(300, c(100, 75, 50, 25) + 7, labels = c("1", "0.75", "0.5", "0.25"),
     cex = 0.75)
text(0.25, 105, "Emax ratio", cex = 0.75, pos = 4)

# 겉보기 EC50 (효과가 원래 Emax 의 절반에 이르는 농도)을 곡선에서 읽으면
sapply(ratio, function(r) CA[which.max(Emax.model(CA, 100, 5*(1 + r)) >= 50)])
