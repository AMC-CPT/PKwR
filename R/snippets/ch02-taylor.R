# Taylor 전개: 복잡한 함수를 한 점 주변에서 다항식으로 바꾼다
tay <- function(x, n) switch(n, 1 + x, 1 + x + x^2/2, 1 + x + x^2/2 + x^3/6)
xs <- c(0.05, 0.2, 0.5, 1.0)
round(rbind(exact = exp(xs), order1 = tay(xs, 1),
            order2 = tay(xs, 2), order3 = tay(xs, 3)), 5)
round(rbind(err1 = abs(tay(xs, 1) - exp(xs)),
            err2 = abs(tay(xs, 2) - exp(xs))), 6)

xg <- seq(-1, 1.6, 0.005)
plot(xg, exp(xg), type = "l", lwd = 1.6, las = 1, bty = "l", ylim = c(0, 5),
     xlab = "x", ylab = expression(e^x))
lines(xg, tay(xg, 1), lty = 2); lines(xg, tay(xg, 2), lty = 3)
abline(v = 0, lty = 3, col = "gray60")
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 2, 3), lwd = c(1.6, 1, 1),
       legend = c("exp(x)", "일차 전개", "이차 전개"))
