# 기저치가 일주기로 흔들리는 계: kin 이 24시간 주기의 코사인을 따른다
library(deSolve)
kin0 <- 10; kout.b <- 0.5; amp <- 0.30; tpk <- 6    # 진폭 30%, 정점 06시
Cp.b <- function(t) ifelse(t < 12, 0, 25*exp(-0.20*(t - 12)))   # 12시에 투여
kin.f <- function(t) kin0*(1 + amp*cos(2*pi*(t - tpk)/24))
dbase <- function(t, y, p) {                        # 약물은 kin 을 억제한다
  I <- 0.75*Cp.b(t)/(4 + Cp.b(t))
  list(kin.f(t)*(1 - p[["drug"]]*I) - kout.b*y[1])
}
tb  <- seq(0, 48, 0.1)
R0b <- kin0/kout.b
pbo <- lsoda(c(R = R0b), tb, dbase, c(drug = 0))[, "R"]   # 위약(기저) 곡선
act <- lsoda(c(R = R0b), tb, dbase, c(drug = 1))[, "R"]   # 실약 곡선

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(tb, pbo, type = "l", lty = 3, las = 1, bty = "l", ylim = c(5, 30),
     xlab = "Time (hr)", ylab = "Response", main = "(a) observed")
lines(tb, act); abline(h = R0b, lty = 2, col = "gray60")
legend("bottomleft", bty = "n", cex = 0.75, lty = c(1, 3, 2),
       col = c(1, 1, "gray60"),
       legend = c("active", "placebo", "constant baseline"))

plot(tb, act - R0b, type = "l", lty = 2, las = 1, bty = "l", ylim = c(-14, 9),
     xlab = "Time (hr)", ylab = "Change from baseline",
     main = "(b) baseline correction")
lines(tb, act - pbo); abline(h = 0, lty = 3)
legend("topright", bty = "n", cex = 0.75, lty = c(1, 2),
       legend = c("vs placebo (correct)", "vs constant (biased)"))

# 투여 전 구간(0-12h)에서 이미 기저가 움직인다: 상수 기저로 보정하면
# 약효가 없는데도 '효과'가 보이고, 최저점의 크기와 시각이 함께 왜곡된다
i.pre <- tb <= 12; i.pos <- tb > 12
round(c(pre.range = diff(range(pbo[i.pre])),
        nadir.vs.const = min(act[i.pos] - R0b),
        nadir.vs.pbo   = min(act[i.pos] - pbo[i.pos]),
        t.nadir.const  = tb[i.pos][which.min(act[i.pos] - R0b)],
        t.nadir.pbo    = tb[i.pos][which.min(act[i.pos] - pbo[i.pos])]), 3)
