# 1구획, 비영 초기조건: 한 구간을 전진시키는 함수. 세 항의 합이다.
#   제차해(남아 있던 약물) + 주입의 특수해 + 흡수부위의 특수해
adv1 <- function(Xa0, X0, R, Ke, Ka, dt) {
  c(Xa = Xa0*exp(-Ka*dt),
    X  = X0*exp(-Ke*dt) + R/Ke*(1 - exp(-Ke*dt)) +
         Ka*Xa0/(Ka - Ke)*(exp(-Ke*dt) - exp(-Ka*dt)))
}

# 투여력을 따라 구간마다 전진한다. 기록하는 값은 그 시각의 용량을 넣기 전이다.
run1 <- function(DH, Ke, Ka) {
  M  <- matrix(0, nrow(DH), 2, dimnames = list(NULL, c("Xa", "X")))
  st <- c(Xa = 0, X = 0)
  for (i in seq_len(nrow(DH))) {
    if (i > 1) st <- adv1(st[1], st[2], DH$RATE2[i - 1], Ke, Ka,
                          DH$TIME[i] - DH$TIME[i - 1])
    M[i, ] <- st
    if (DH$BOLUS[i] > 0) st[DH$CMT[i]] <- st[DH$CMT[i]] + DH$BOLUS[i]
  }
  M
}

M1 <- run1(dh2, Ke = 0.1, Ka = 1)
round(cbind(TIME = dh2$TIME, M1), 4)

# wnl 의 Comp1 과 대조한다
max(abs(M1 - Comp1(Ke = 0.1, Ka = 1, dh2)))
