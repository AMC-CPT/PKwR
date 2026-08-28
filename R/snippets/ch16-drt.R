# 용량-반응-시간(DRT): 혈장농도 없이 용량과 반응-시간만으로 적합한다.
# 생체상(biophase)의 양을 1차 흡수-1차 소실로 두고 그것이 Imax 모형을
# 구동한다고 가정한다. 농도 척도가 없으므로 ID50 은 '용량' 단위다.
set.seed(20260828)
Dd <- c(1, 3, 10); E0d <- 20; Imx <- 14; ID50 <- 2.5; Hd <- 1.5
kad <- 0.9; ked <- 0.15                              # /hr (참값)
biop <- function(t, D, ka, ke) D*ka/(ka - ke)*(exp(-ke*t) - exp(-ka*t))
td <- c(0.5, 1, 2, 3, 4, 6, 8, 12, 18, 24)
dD <- do.call(rbind, lapply(Dd, function(d) {
  A <- biop(td, d, kad, ked)
  data.frame(DOSE = d, TIME = td,
             DV = round(E0d - Imx*A^Hd/(ID50^Hd + A^Hd) + rnorm(length(td), 0, 0.4), 2))
}))

library(wnl)
fD <- function(TH) {                                 # 6개 파라미터 동시 추정
  A <- biop(dD$TIME, dD$DOSE, TH[1], TH[2])
  TH[3] - TH[4]*A^TH[6]/(TH[5]^TH[6] + A^TH[6])
}
rD <- nlr(fD, dD, pNames = c("Ka", "Ke", "E0", "Imax", "ID50", "H"),
          IE = c(0.5, 0.1, 20, 12, 2, 1), Error = "A",
          LB = c(0.05, 0.01, 5, 1, 0.1, 0.3), UB = c(20, 5, 50, 40, 50, 6))
round(rD$Est[c("PE", "RSE"), 1:6], 3)
rbind(true = c(Ka = kad, Ke = ked, E0 = E0d, Imax = Imx, ID50 = ID50, H = Hd))

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
pe <- rD$Est["PE", ]; tg <- seq(0.1, 24, 0.1)
plot(dD$TIME, dD$DV, las = 1, bty = "l", pch = rep(c(16, 17, 15), each = 10),
     ylim = c(4, 22), xlab = "Time (hr)", ylab = "Response",
     main = "(a) response vs time")
for (i in 1:3) {
  A <- biop(tg, Dd[i], pe[["Ka"]], pe[["Ke"]])
  lines(tg, pe[["E0"]] - pe[["Imax"]]*A^pe[["H"]]/(pe[["ID50"]]^pe[["H"]] + A^pe[["H"]]),
        lty = i)
}
legend("bottomright", bty = "n", cex = 0.8, lty = 1:3, pch = c(16, 17, 15),
       legend = paste("D =", Dd))

# 시간을 지우면 남는 것: 추정된 생체상 양과 반응의 관계(항정상태 곡선)
Ag <- 10^seq(-1, 1, 0.01)
Ehat <- function(A, p) p[["E0"]] - p[["Imax"]]*A^p[["H"]]/(p[["ID50"]]^p[["H"]] + A^p[["H"]])
plot(Ag, Ehat(Ag, pe), type = "l", log = "x", las = 1, bty = "l", ylim = c(4, 22),
     xlab = "Biophase amount (est.)", ylab = "Response",
     main = "(b) recovered E-A curve")
lines(Ag, E0d - Imx*Ag^Hd/(ID50^Hd + Ag^Hd), lty = 2, col = "gray55", lwd = 2)
abline(v = pe[["ID50"]], lty = 3)
legend("bottomleft", bty = "n", cex = 0.8, lty = c(1, 2), lwd = c(1, 2),
       col = c(1, "gray55"), legend = c("estimated", "true"))
