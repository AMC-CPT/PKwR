# 성장-사멸 모형: 증식 중인 세포를 약물이 죽이되, 죽음이 곧바로
# 관측되지 않고 전달구획을 지나 나타난다 (항암제 종양, 항균제 균수).
library(deSolve)
kg <- 0.03; kmax <- 0.45; EC50k <- 2; ktr <- 0.15    # /hr, mg/L
Cp.k <- function(t, D) D/10*exp(-0.12*t)             # 1구획 정맥, t1/2 5.8 hr
dkill <- function(t, y, p) {
  Ck <- Cp.k(t, p[["D"]])
  kk <- kmax*Ck/(EC50k + Ck)                         # 농도 의존 사멸속도
  c(list(c(kg*y[1] - kk*y[1],                        # 증식 구획
           kk*y[1] - ktr*y[2], ktr*(y[2] - y[3]),    # 사멸 전달 사슬
           ktr*(y[3] - y[4]))), total = sum(y))
}
tk <- seq(0, 240, 0.5); Dk <- c(0, 30, 100, 300)
Nk <- sapply(Dk, function(d)
        lsoda(c(100, 0, 0, 0), tk, dkill, c(D = d))[, "total"])

matplot(tk, Nk, type = "l", lty = 1:4, col = 1, log = "y", las = 1, bty = "l",
        xlab = "Time (hr)", ylab = "Cell number (% of initial)",
        ylim = c(1, 6000))
abline(h = 100, lty = 3)
legend("top", bty = "n", cex = 0.75, lty = 1:4, horiz = TRUE,
       legend = paste("D =", Dk))

# 정지 농도(stasis concentration): 사멸이 증식과 같아져 순증식이 0이 되는 농도
Cst <- EC50k*kg/(kmax - kg)
res.k <- rbind(nadir.pct = apply(Nk, 2, min), t.nadir = tk[apply(Nk, 2, which.min)],
               C0.over.Cst = Dk/10/Cst)
colnames(res.k) <- paste("D =", Dk)
round(c(C.stasis = Cst), 3); round(res.k, 2)
