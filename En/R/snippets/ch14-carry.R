# With carry-over added, the four cells of a 2x2 have one parameter too many.
# Contrast columns (+-1): sequence, period, formulation, carry-over (period 2 only).
d22 <- data.frame(SEQ = c( 1,  1, -1, -1),      # RT = +1, TR = -1
                  PRD = c(-1,  1, -1,  1),      # period 1 = -1, period 2 = +1
                  TRT = c(-1,  1,  1, -1),      # R = -1, T = +1
                  CAR = c( 0, -1,  0,  1))      # period 2 only: C_R = -1, C_T = +1
X22 <- with(d22, cbind(int = 1, seq = SEQ, prd = PRD, trt = TRT, car = CAR))
X22
c(rank = qr(X22)$rank, columns = ncol(X22))

# Carry-over column = linear combination of sequence and formulation (not separable)
round(coef(lm(CAR ~ 0 + SEQ + TRT, d22)), 3)

# If carry-over differs between formulations, the formulation estimate is biased by
# that much. Usual 2x2 estimate on true values: (RT period diff - TR period diff)/2.
Pe <- 0.10; Fe <- 0                             # period, true formulation effect (T-R)
cR <- -0.06; cT <- 0.06                         # C_T - C_R = 0.12
cell <- c(RT1 = -Pe/2 - Fe/2,        RT2 =  Pe/2 + Fe/2 + cR,
          TR1 = -Pe/2 + Fe/2,        TR2 =  Pe/2 - Fe/2 + cT)
Fhat <- ((cell[["RT2"]] - cell[["RT1"]]) - (cell[["TR2"]] - cell[["TR1"]]))/2
round(c(true.F = Fe, estimated.F = Fhat, bias = Fhat - Fe,
        minus.half.dC = -(cT - cR)/2), 4)

# In a replicate design carry-over is separable: 2x4 (TRTR | RTRT)
d24 <- data.frame(SEQ = rep(c(1, -1), each = 4),
                  PRD = rep(c(-1.5, -0.5, 0.5, 1.5), 2),
                  TRT = c( 1, -1,  1, -1,  -1,  1, -1,  1),
                  CAR = c( 0,  1, -1,  1,   0, -1,  1, -1))
X24 <- with(d24, cbind(int = 1, seq = SEQ, prd = PRD, trt = TRT, car = CAR))
c(rank = qr(X24)$rank, columns = ncol(X24))
