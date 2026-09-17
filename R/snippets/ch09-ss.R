# 선형계에서 항정상태 곡선은 단회 곡선의 중첩(superposition)이다
tau <- 12                                      # 투여간격 (hr)
Css <- function(t, n = 40) {                   # n 회 투여 뒤의 농도
  s <- (0:(n - 1))*tau                         # 투여 시각들
  sapply(t, function(u) sum(C2iv(u - s[s <= u])))
}
tss <- c(0, 5/60, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12)  # 마지막 간격의 채혈표
t40 <- 39*tau                                  # 마지막(40번째) 투여 시각
Cs  <- Css(t40 + tss)                          # tss=0 은 투여 직후, 12 는 다음 투여 직전

# 한 투여간격의 지표
AUCtau  <- auc.ld(tss, Cs)
Cmax.ss <- max(Cs); Cmin.ss <- min(Cs); Cav <- AUCtau/tau
round(c(AUCtau = AUCtau, Cmax.ss = Cmax.ss, Cmin.ss = Cmin.ss, Cav = Cav,
        PTF.pct = (Cmax.ss - Cmin.ss)/Cav*100, swing = (Cmax.ss - Cmin.ss)/Cmin.ss), 4)

# 선형계의 항등식: AUC(tau, ss) = AUC(0-inf, 단회) = D/CL
round(c(AUCtau.ss = AUCtau, AUCinf.single = integrate(C2iv, 0, Inf)$value,
        D.over.CL = D/CL), 4)

# 축적비는 하나가 아니다. 지표마다 다르고 1구획 공식은 그중 어느 것도 아니다
round(c(Rac.AUC  = AUCtau/auc.ld(tss, C2iv(tss)),
        Rac.Cmax = Cmax.ss/C2iv(0),
        Rac.Cmin = Cmin.ss/C2iv(tau),
        onecomp  = 1/(1 - exp(-lam*tau))), 4)

# NonCompart 로 같은 계산: SS = TRUE 는 무한대 외삽을 끄고 CL = D/AUCtau 로 간다
library(NonCompart)
r.ss <- sNCA(tss, Cs, dose = D, adm = "Bolus", down = "Log", SS = TRUE, tau = tau,
             doseUnit = "mg", concUnit = "mg/L")
round(r.ss[c("CMAX", "TMAX", "CLST", "AUCLST", "LAMZHL", "CLO", "VZO", "VSSO")], 4)

# 부분 AUC: 구간을 직접 지정한다 (합이 AUCtau 와 맞는지가 검산이다)
iA <- data.frame(Name = c("AUC[0-2h]", "AUC[2-12h]"), Start = c(0, 2), End = c(2, 12))
r.p <- sNCA(tss, Cs, dose = D, adm = "Bolus", down = "Log", SS = TRUE, tau = tau, iAUC = iA)
round(r.p[c("AUC[0-2h]", "AUC[2-12h]", "AUCLST")], 4)
