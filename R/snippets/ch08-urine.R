# 소변 자료의 NCA: 구간별 배설량 Ae 에서 신청소율과 fe 를 얻는다.
# 참 모형에서 fe = 0.4 (CLr = 1.8 L/hr) 로 소변 자료를 만든다.
fe.true <- 0.4; CLr.true <- fe.true*CL
tU  <- c(0, 4, 8, 12, 24, 48)                       # 수집 구간의 경계 (hr)
AeI <- sapply(2:length(tU), function(i)             # 구간 배설량의 참값 (mg)
         CLr.true*integrate(C2iv, tU[i - 1], tU[i])$value)
set.seed(20260827)                                  # 측정오차 5% (로그정규)
AeI <- AeI*exp(rnorm(length(AeI), 0, 0.05))
Ae  <- cumsum(AeI)
round(rbind(t.end = tU[-1], Ae.interval = AeI, Ae.cum = Ae), 2)

# 신청소율: 같은 기간(0-48 h)의 혈장 AUC 로 나눈다
CLr <- Ae[length(Ae)]/AUC.lst
# fe: 소박한 Ae/D 는 48시간 이후의 꼬리를 놓친다.
#     fe = CLr/CL = (Ae/D) x (AUCinf/AUClast) 로 보정한다.
round(c(CLr = CLr, fe.naive = Ae[length(Ae)]/D,
        fe = Ae[length(Ae)]/D*AUC.inf/AUC.lst, fe.true = fe.true), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
plot(tU[-1], Ae, las = 1, bty = "l", pch = 16, xlim = c(0, 48),
     ylim = c(0, 115), xlab = "Time (hr)", ylab = "Cumulative Ae (mg)",
     main = "(a) cumulative excretion")
lines(tU[-1], Ae, lty = 1)
abline(h = fe.true*D, lty = 3)
text(1, fe.true*D + 7, "fe x Dose", cex = 0.8, pos = 4)

mid  <- (head(tU, -1) + tU[-1])/2                   # 구간의 중간 시각
rate <- AeI/diff(tU)                                # 배설 속도 (mg/hr)
plot(C2iv(mid), rate, las = 1, bty = "l", pch = 16, log = "xy",
     xlab = "C at interval midpoint (mg/L)", ylab = "Excretion rate (mg/hr)",
     main = "(b) rate vs concentration")
abline(a = log10(CLr.true), b = 1, lty = 3)         # 기울기 = 신청소율
