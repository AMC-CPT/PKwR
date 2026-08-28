# 간청소율의 세 모형은 같은 CLint 에서 서로 다른 추출률을 준다. R = fu*CLint/QH
er.ws <- function(R) R/(1 + R)                    # well-stirred (잘 교반)
er.pt <- function(R) 1 - exp(-R)                  # parallel-tube (평행관)
er.dp <- function(R, Dn = 0.17) {                 # dispersion (분산), Dn: 분산수
  a <- sqrt(1 + 4*R*Dn)
  1 - 4*a/((1 + a)^2*exp((a - 1)/(2*Dn)) - (1 - a)^2*exp(-(a + 1)/(2*Dn))) }

Rn <- c(0.05, 0.25, 1, 4, 20)
round(rbind(R = Rn, well.stirred = er.ws(Rn), parallel.tube = er.pt(Rn),
            dispersion = sapply(Rn, er.dp)), 4)

# 실무의 함의는 추출률이 아니라 F = 1 - ER 에서 드러난다
round(c(F.ws = 1 - er.ws(10), F.pt = 1 - er.pt(10),
        ratio = (1 - er.ws(10))/(1 - er.pt(10))), 6)

Rg <- 10^seq(-2, 1.5, 0.02)
matplot(Rg, cbind(1 - er.ws(Rg), 1 - er.pt(Rg), 1 - sapply(Rg, er.dp)),
        type = "l", log = "xy", lty = 1:3, col = 1, las = 1, bty = "l",
        ylim = c(1e-4, 1.2), xlab = expression(f[u]*CL[int]/Q[H]),
        ylab = "F = 1 - ER", yaxt = "n")
axis(2, at = 10^(-4:0), labels = c("0.0001", "0.001", "0.01", "0.1", "1"), las = 1)
legend("bottomleft", bty = "n", cex = 0.85, lty = 1:3,
       legend = c("well-stirred", "parallel-tube", "dispersion"))
