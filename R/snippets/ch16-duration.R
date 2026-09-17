# 1구획 IV 일시주입 뒤의 직접효과. 농도는 반감기 하나의 시계로 떨어지지만
# 효과는 포화 때문에 다른 시계로 떨어진다.  r0 = C0/EC50 (초기농도/EC50 비)
k  <- 0.3; EC50 <- 2; thalf <- log(2)/k           # 제거 반감기 2.31 hr
r0 <- c(1, 4, 16, 64)
t  <- seq(0, 24, 0.005)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
matplot(t, sapply(r0, function(r) r*exp(-k*t)), type = "l", log = "y",
        lty = 1:4, col = 1, las = 1, bty = "l", ylim = c(0.1, 100), yaxt = "n",
        xlab = "Time (hr)", ylab = "C / EC50", main = "(a) concentration")
axis(2, at = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"), las = 1)
abline(h = 1, lty = 3); text(21, 1.4, "EC50", cex = 0.8)

matplot(t, sapply(r0, function(r) 100*r*exp(-k*t)/(1 + r*exp(-k*t))),
        type = "l", lty = 1:4, col = 1, las = 1, bty = "l", ylim = c(0, 100),
        xlab = "Time (hr)", ylab = "Effect (% of Emax)", main = "(b) effect")
legend("topright", bty = "n", cex = 0.8, lty = 4:1, legend = paste0("r0 = ", rev(r0)))

# (1) 효과가 50% (C > EC50) 위에 머무는 시간: 용량이 두 배가 될 때마다
#     한 반감기씩 늘어난다.
# (2) 효과가 처음의 절반으로 줄어드는 시각(반감기 단위): log2(2 + r0)
t50 <- sapply(r0, function(r) {                   # (2)를 수치로 확인
  E <- 100*r*exp(-k*t)/(1 + r*exp(-k*t))
  t[which.max(E <= E[1]/2)]
})
round(rbind(dur.over.50.thalf = log(r0)/k/thalf,
            t50E.thalf = t50/thalf, theory.log2 = log2(2 + r0)), 3)
