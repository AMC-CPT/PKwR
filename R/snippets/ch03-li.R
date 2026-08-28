# 분산의 구간추정 세 가지. lh 는 R 에 내장된 황체형성호르몬 자료(48점)다.
library(LBI)                                  # 저자의 우도 기반 추론 패키지
n1 <- length(lh); v1 <- var(lh); cl <- 0.95
ci  <- (n1 - 1)*v1/qchisq(c(0.5 + cl/2, 0.5 - cl/2), n1 - 1)   # 통상의 카이제곱 CI
lb  <- LBCIvar(lh,   conf.level = cl)["var", c("LL", "UL")]    # 우도기반 CI
li  <- LInormVar(lh, conf.level = cl)["var", c("LL", "UL")]    # 우도구간
w   <- rbind(CI = ci, LBCI = lb, LI = li)
round(cbind(w, width = w[, 2] - w[, 1]), 4)

# 왜 다른가: 우도 곡선 위에서 두 끝점의 높이를 비교한다
LL <- function(v) -n1/2*(log(2*pi*v) + (n1 - 1)*v1/(n1*v))     # 프로파일 로그우도
h  <- c(LI.LL = LL(li[[1]]), LI.UL = LL(li[[2]]),
        CI.LL = LL(ci[1]),   CI.UL = LL(ci[2])) - LL(v1)
round(h, 4)

vg <- seq(0.15, 0.60, 0.001)
plot(vg, LL(vg) - LL(v1), type = "l", las = 1, bty = "l", ylim = c(-5, 0.4),
     xlab = "분산", ylab = "로그우도 (최댓값 기준)")
abline(v = v1, lty = 3); abline(h = h[["LI.LL"]], lty = 3, col = "gray55")
segments(li, -5, li, h[1:2])                                   # LI 의 두 끝
segments(ci, -5, ci, h[3:4], lty = 2)                          # CI 의 두 끝
legend(0.33, -3.0, bty = "n", cex = 0.8, lty = c(1, 2),
       legend = c("LI: 두 끝의 높이가 같다", "CI: 높이가 다르다"))
