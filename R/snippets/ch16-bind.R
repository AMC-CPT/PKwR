# 포화 결합 실험: 총 결합 = 특이 결합(포화) + 비특이 결합(선형)
set.seed(20260828)
Lg <- c(0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256)      # 자유 리간드 (nM)
Bm.t <- 2.5; Kd.t <- 8; ns.t <- 0.004               # 참값
dB <- data.frame(Lg = Lg, DV = round((Bm.t*Lg/(Kd.t + Lg) + ns.t*Lg)*
                                     exp(rnorm(length(Lg), 0, 0.05)), 4))

library(wnl)                                        # 7장에서 쓴 비선형 회귀
fB <- function(TH) TH[1]*dB$Lg/(TH[2] + dB$Lg) + TH[3]*dB$Lg
rB <- nlr(fB, dB, pNames = c("Bmax", "Kd", "NS"), IE = c(2, 10, 0.01),
          LB = c(0.1, 0.1, 0), UB = c(20, 500, 1), Error = "P")
round(rB$Est[c("PE", "RSE"), ], 4)

# Scatchard 변환(B/L 대 B)은 비특이 결합을 빼기 전에는 직선이 되지 않는다
Bsp <- dB$DV - rB$Est["PE", "NS"]*dB$Lg             # 특이 결합만 남기기
sc  <- coef(lm(I(Bsp/dB$Lg) ~ Bsp))                 # 기울기 = -1/Kd
round(c(Kd.scatchard = -1/sc[[2]], Bmax.scatchard = -sc[[1]]/sc[[2]],
        Kd.nls = rB$Est["PE", "Kd"], Bmax.nls = rB$Est["PE", "Bmax"]), 3)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(dB$Lg, dB$DV, log = "x", las = 1, bty = "l", pch = 16, ylim = c(0, 3.6),
     xlab = "Free ligand (nM)", ylab = "Bound", main = "(a) saturation")
curve(rB$Est["PE", "Bmax"]*x/(rB$Est["PE", "Kd"] + x) +
      rB$Est["PE", "NS"]*x, 0.3, 300, add = TRUE)
curve(rB$Est["PE", "Bmax"]*x/(rB$Est["PE", "Kd"] + x), 0.3, 300,
      add = TRUE, lty = 2)
curve(rB$Est["PE", "NS"]*x, 0.3, 300, add = TRUE, lty = 3)
legend("topleft", bty = "n", cex = 0.75, lty = 1:3,
       legend = c("total", "specific", "nonspecific"))

plot(Bsp, Bsp/dB$Lg, las = 1, bty = "l", pch = 16, xlim = c(0, 2.6),
     xlab = "Bound (specific)", ylab = "Bound / Free", main = "(b) Scatchard")
abline(sc, lty = 2)
points(dB$DV, dB$DV/dB$Lg, pch = 1)                 # 비특이 결합을 두면 휜다
legend("topright", bty = "n", cex = 0.75, pch = c(16, 1),
       legend = c("NS-corrected", "uncorrected"))

# 치환 실험: 표지 리간드 농도 L* 아래의 IC50 에서 Ki 를 얻는다 (Cheng-Prusoff)
Lst <- 5                                            # 표지 리간드 5 nM
round(c(IC50 = 30, Ki = 30/(1 + Lst/Kd.t)), 3)
