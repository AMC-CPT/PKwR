# 사다리꼴 공식 두 가지: 선형(linear)과 로그(log)
auc.lin <- function(t, C) sum(diff(t)*(head(C, -1) + tail(C, -1))/2)
auc.ld  <- function(t, C) {                    # linear-up log-down
  C1 <- head(C, -1); C2 <- tail(C, -1)
  dn <- C2 < C1 & C2 > 0                       # 하강 구간만 로그
  sum(ifelse(dn, (C1 - C2)/log(C1/C2), (C1 + C2)/2)*diff(t))
}

# 잡음 없는 참 곡선을 관측 시각에서만 취해 방법 자체의 편향을 본다
Ct  <- C2iv(tobs)
ref <- integrate(C2iv, tobs[1], 48)$value      # 참 AUC (첫 채혈 이후)
round(c(true = ref, linear = auc.lin(tobs, Ct), lin.log = auc.ld(tobs, Ct)), 3)

# 관측(잡음 포함) 자료에 적용
round(c(AUClast.lin = auc.lin(dat2$Time, dat2$DV),
        AUClast.ld  = auc.ld (dat2$Time, dat2$DV)), 3)
