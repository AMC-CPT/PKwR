# 잔차가 보내는 두 가지 신호: 깔때기(오차 모형)와 파형(구조 모형)
set.seed(20260828)                            # 참으로 2지수인 자료를 따로 만든다
x2 <- c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 9, 12, 18, 24)
y2 <- round((70*exp(-1.2*x2) + 40*exp(-0.12*x2))*
            exp(rnorm(length(x2), 0, 0.07)), 3)
b1 <- optim(c(100, 0.3), function(p) sum((log(y2) - log(p[1]*exp(-p[2]*x2)))^2),
            method = "L-BFGS-B", lower = c(1, .01), upper = c(1e4, 20))$par

par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))
f.ols <- o1[1]*exp(-o1[2]*d4$x)
plot(f.ols, d4$DV - f.ols, las = 1, bty = "l", pch = 16, xlab = "적합값",
     ylab = "잔차", main = "(a) 등가중, 원잔차"); abline(h = 0, lty = 3)
f.els <- o3[1]*exp(-o3[2]*d4$x)
plot(f.els, (d4$DV - f.els)/f.els, las = 1, bty = "l", pch = 16, ylim = c(-.3, .3),
     xlab = "적합값", ylab = "상대잔차", main = "(b) 비례오차, 가중잔차")
abline(h = 0, lty = 3)
f.mis <- b1[1]*exp(-b1[2]*x2)
plot(x2, (y2 - f.mis)/f.mis, las = 1, bty = "l", pch = 16,
     xlab = "x", ylab = "상대잔차", main = "(c) 모형 오설정")
abline(h = 0, lty = 3); lines(x2, (y2 - f.mis)/f.mis, lty = 2)
