# Full two-compartment solution = homogeneous + infusion particular + oral particular
# (equation (eq:lapall)). Roots lam and coefficients Co come from ch08-expat. The form
# is independent of the number of compartments, so it is reused for three compartments.
Ka <- 1
advN <- function(st, R, dt, L, Cf) {          # st = c(absorption site, cmt 1..n)
  eL   <- exp(-L*dt)
  eA   <- Reduce(`+`, Map(function(C, e) C*e, Cf, eL))     # exp(A dt)
  col1 <- sapply(Cf, function(C) C[, 1])                   # input enters compartment 1
  c(st[1]*exp(-Ka*dt),
    eA %*% st[-1]                               +          # homogeneous
    R*(col1 %*% ((1 - eL)/L))                   +          # infusion
    Ka*st[1]*(col1 %*% ((eL - exp(-Ka*dt))/(Ka - L))))     # oral
}
runN <- function(DH, L, Cf) {
  M  <- matrix(0, nrow(DH), length(L) + 1,
               dimnames = list(NULL, c("Xg", paste0("X", seq_along(L)))))
  st <- numeric(length(L) + 1)
  for (i in seq_len(nrow(DH))) {
    if (i > 1) st <- advN(st, DH$RATE2[i - 1], DH$TIME[i] - DH$TIME[i - 1], L, Cf)
    M[i, ] <- st                                           # value before the dose
    if (DH$BOLUS[i] > 0) st[DH$CMT[i]] <- st[DH$CMT[i]] + DH$BOLUS[i]
  }
  M
}

M2 <- runN(dh2, lam, Co)
round(cbind(TIME = dh2$TIME, M2)[c(1, 2, 7, 8, 10, 13, 14, 15, 19), ], 4)

# Cross-check against nComp of wnl and numerical integration by deSolve (no-dose times).
library(deSolve)
dydt2 <- function(t, y, pr) list(c(-Ka*y[1],
                                   Ka*y[1] - (K10 + K12)*y[2] + K21*y[3] + pr$R(t),
                                   K12*y[2] - K21*y[3]))
evt <- data.frame(var = c("X1", "Xg"), time = c(0, 48), value = 100, method = "add")
num <- ode(c(Xg = 0, X1 = 0, X2 = 0), dh2$TIME, dydt2,
           list(R = approxfun(c(0, 24, 27, 60), c(0, 50, 0, 0),
                              method = "constant", rule = 2)),
           events = list(data = evt), rtol = 1e-10, atol = 1e-10)
nod <- dh2$BOLUS == 0
c(vs.nComp   = max(abs(M2 - nComp(SolComp2(K10, K12, K21), Ka = Ka, dh2))),
  vs.deSolve = max(abs(M2[nod, 2] - num[nod, 3])))
