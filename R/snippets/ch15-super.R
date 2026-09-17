# 단회 농도곡선: 다지수함수 (2구획 일시주입꼴)
Csd <- function(t) 2.2*exp(-0.45*t) + 0.9*exp(-0.10*t)

# 중첩의 원리: 선형이면 반복투여 곡선은 단회 곡선을 시간이동하여 더한 것
superpose <- function(f, t, tau, n)
  rowSums(sapply(0:(n - 1), function(i) ifelse(t < i*tau, 0, f(t - i*tau))))

tau <- 12; nd <- 12
AUC.sd <- integrate(Csd, 0, Inf)$value                       # 단회 0-inf
AUC.ss <- integrate(function(t) superpose(Csd, t, tau, nd),  # 항정상태 0-tau
                    (nd - 1)*tau, nd*tau)$value
round(c(AUC.inf.single = AUC.sd, AUC.tau.ss = AUC.ss), 4)

t <- seq(0, 144, 0.05)
Cm <- superpose(Csd, t, tau, nd)
plot(t, Cm, type = "n", las = 1, bty = "l", ylim = c(0, 4.6),
     xlab = "Time (hr)", ylab = "Concentration")
i.ss <- t >= (nd - 1)*tau                                    # 마지막 투여간격
polygon(c(t[i.ss], rev(t[i.ss])), c(Cm[i.ss], 0*t[i.ss]),
        col = "gray85", border = NA)
polygon(c(t, rev(t)), c(Csd(t), 0*t), density = 14, angle = 60, border = NA)
lines(t, Cm); lines(t, Csd(t), lty = 2)
points(seq(0, (nd - 1)*tau, tau), rep(0, nd), pch = 17, cex = 0.55)
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("multiple dose (q12h)", "single dose"))
