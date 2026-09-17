# 포화되는 단백결합: 유리농도는 용량에 비례하지만 총농도는 그렇지 않다.
Bmax <- 100; Kdb <- 15                            # 결합 자리(mg/L)와 해리상수
CLintb <- 2                                       # 저추출 약물의 내적 청소율 L/hr
Ctot <- function(Cf) Cf + Bmax*Cf/(Kdb + Cf)      # 총 = 유리 + 결합
R0b   <- c(1, 2, 5, 10, 20, 40)                   # 지속정주 속도 (mg/hr)
Cfree <- R0b/CLintb                               # 항정상태: 제거는 유리약물에 비례
round(rbind(rate = R0b, C.free = Cfree, C.total = Ctot(Cfree),
            fu = Cfree/Ctot(Cfree),
            total.per.rate = Ctot(Cfree)/R0b,
            free.per.rate = Cfree/R0b), 4)

Rg <- seq(0, 45, 0.1); Cg <- Rg/CLintb
plot(Rg, Ctot(Cg), type = "l", las = 1, bty = "l", xlab = "Infusion rate (mg/hr)",
     ylab = "Steady-state concentration (mg/L)", ylim = c(0, 130))
lines(Rg, Cg, lty = 2)
lines(Rg, Ctot(Cg[Rg == 5])/5*Rg, lty = 3, col = "gray55")   # 저용량 비례선
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 2, 3),
       col = c(1, 1, "gray55"), legend = c("총농도", "유리농도", "저용량 비례선"))
