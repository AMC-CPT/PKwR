# One compartment, non-zero initial condition: function advancing one interval. It sums
#   homogeneous (remaining drug) + infusion particular + absorption-site particular
adv1 <- function(Xa0, X0, R, Ke, Ka, dt) {
  c(Xa = Xa0*exp(-Ka*dt),
    X  = X0*exp(-Ke*dt) + R/Ke*(1 - exp(-Ke*dt)) +
         Ka*Xa0/(Ka - Ke)*(exp(-Ke*dt) - exp(-Ka*dt)))
}

# Advance interval by interval along the dosing history; record values before the dose.
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

# Cross-check against Comp1 of wnl
max(abs(M1 - Comp1(Ke = 0.1, Ka = 1, dh2)))
