# 겉보기 분포용적 Vd(t) = 체내 총량/혈장농도 는 V1 에서 Vz 로 커진다
k12 <- Q/V1
X1 <- function(t) V1*C2iv(t)                        # 중심구획 약물량
X2 <- function(t) D*k12/(th[["alpha"]] - th[["beta"]]) *
                  (exp(-th[["beta"]]*t) - exp(-th[["alpha"]]*t))
t  <- seq(0, 12, 0.01)
Vd <- (X1(t) + X2(t))/C2iv(t)

plot(t, Vd, type = "l", las = 1, bty = "l", ylim = c(0, 78),
     xlab = "Time (hr)", ylab = "Apparent Vd (L)")
abline(h = c(V1, V1 + V2, CL/th[["beta"]]), lty = 3)
text(rep(11.6, 3), c(V1, V1 + V2 - 4.5, CL/th[["beta"]]) + 3.8, cex = 0.85,
     labels = c(expression(V[1]), expression(V[ss]), expression(V[z])))
