# 병용의 정량: Loewe 상가성은 두 약을 '서로의 희석'으로 본다.
# 같은 효과를 내는 조합이 CA/EA + CB/EB = 1 을 만족하면 상가이고,
# 상호작용 항 psi 를 더하면 상승(psi > 0)과 길항(psi < 0)이 된다.
EA <- 4; EB <- 10                                    # 단독으로 목표 효과를 내는 농도
iso <- function(CA, psi) {                           # 목표 효과의 등효과선
  b <- 1 - CA/EA
  EB*b/(1 + psi*CA/EA)
}
CAg <- seq(0, EA, length.out = 200)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CAg, iso(CAg, 0), type = "l", las = 1, bty = "l", ylim = c(0, EB),
     xlab = "Drug A (mg/L)", ylab = "Drug B (mg/L)", main = "(a) isobologram")
lines(CAg, iso(CAg,  3), lty = 2)
lines(CAg, iso(CAg, -0.6), lty = 4)
legend("topright", bty = "n", cex = 0.8, lty = c(2, 1, 4),
       legend = c("synergy", "additivity", "antagonism"))

# 반응면: 상가 조합의 효과를 Emax 모형으로 계산한다 (효과 단위 합산)
Esurf <- function(a, b, psi) {
  z <- a/EA + b/EB + psi*a*b/(EA*EB)                 # 상가 단위의 총량
  100*z/(1 + z)                                      # 목표 효과 50 을 기준으로
}
ag <- seq(0, 8, length.out = 60); bg <- seq(0, 20, length.out = 60)
contour(ag, bg, outer(ag, bg, Esurf, psi = 3), las = 1, bty = "l",
        levels = c(20, 35, 50, 65, 80), labcex = 0.65,
        xlab = "Drug A (mg/L)", ylab = "Drug B (mg/L)",
        main = "(b) response surface (synergy)")
contour(ag, bg, outer(ag, bg, Esurf, psi = 0), levels = 50, lty = 2,
        drawlabels = FALSE, add = TRUE)

# 상가 기준선 대비 이득: A 와 B 를 절반씩 섞었을 때의 효과
round(c(A.alone = Esurf(EA, 0, 0), B.alone = Esurf(0, EB, 0),
        half.additive = Esurf(EA/2, EB/2, 0),
        half.synergy  = Esurf(EA/2, EB/2, 3),
        half.antagon  = Esurf(EA/2, EB/2, -0.6)), 2)
