# 체내 총량 A(t): 부하용량 0.75 mg 2회(0, 12 hr) + 유지용량 0.25 mg q12h
k <- log(2)/40; tau <- 12
t <- seq(0, 124, 0.05)

A.load <- 0.75*exp(-k*t) + ifelse(t < 12, 0, 0.75*exp(-k*(t - 12)))
A.main <- ifelse(t < 24, 0,
                 0.25*(1 - exp(-(floor(t/tau) - 1)*k*tau))/(1 - exp(-k*tau)) *
                   exp(-k*tprm(t, tau)))
A.with <- A.load + A.main
A.without <- 0.25*acc(t, k, tau)*exp(-k*tprm(t, tau))   # 부하용량 없음

plot(t, A.with, type = "l", las = 1, bty = "l", ylim = c(0, 1.72), xaxt = "n",
     xlab = "투여일", ylab = "체내 digoxin 총량 (mg)")
axis(1, at = seq(0, 120, 24), labels = 0:5)
polygon(c(0, 124, 124, 0), c(1.08, 1.08, 1.34, 1.34),
        col = grey(0.9), border = NA)
lines(t, A.with)
lines(t, A.without, lty = 2)
text(124, 1.66, "치료용량의 범위", pos = 2, cex = 0.85)
text( 56, 0.60, "부하용량 없음",   pos = 4, cex = 0.85)
