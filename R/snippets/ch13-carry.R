# 2x2 의 네 칸에 이월효과까지 넣으면 파라미터가 하나 남는다.
# 열은 순서군 -> 시기 -> 제형 -> 이월의 대비(+-1)이고, 이월은 2기에만 있다.
d22 <- data.frame(SEQ = c( 1,  1, -1, -1),      # RT = +1, TR = -1
                  PRD = c(-1,  1, -1,  1),      # 1기 = -1, 2기 = +1
                  TRT = c(-1,  1,  1, -1),      # R = -1, T = +1
                  CAR = c( 0, -1,  0,  1))      # 2기에만: C_R = -1, C_T = +1
X22 <- with(d22, cbind(int = 1, seq = SEQ, prd = PRD, trt = TRT, car = CAR))
X22
c(rank = qr(X22)$rank, columns = ncol(X22))

# 이월 열은 순서군 열과 제형 열의 선형결합이다 (그래서 분리되지 않는다)
round(coef(lm(CAR ~ 0 + SEQ + TRT, d22)), 3)

# 이월이 두 제형에서 다르면 제형 효과 추정치가 그만큼 치우친다.
# 참값을 넣고 통상적인 2x2 추정(순서군별 시기차의 반차)을 그대로 해 본다.
Pe <- 0.10; Fe <- 0                             # 시기효과, 참 제형효과(T-R)
cR <- -0.06; cT <- 0.06                         # C_T - C_R = 0.12
cell <- c(RT1 = -Pe/2 - Fe/2,        RT2 =  Pe/2 + Fe/2 + cR,
          TR1 = -Pe/2 + Fe/2,        TR2 =  Pe/2 - Fe/2 + cT)
Fhat <- ((cell[["RT2"]] - cell[["RT1"]]) - (cell[["TR2"]] - cell[["TR1"]]))/2
round(c(true.F = Fe, estimated.F = Fhat, bias = Fhat - Fe,
        minus.half.dC = -(cT - cR)/2), 4)

# 반복설계에서는 이월이 분리된다: 2x4 (TRTR | RTRT)
d24 <- data.frame(SEQ = rep(c(1, -1), each = 4),
                  PRD = rep(c(-1.5, -0.5, 0.5, 1.5), 2),
                  TRT = c( 1, -1,  1, -1,  -1,  1, -1,  1),
                  CAR = c( 0,  1, -1,  1,   0, -1,  1, -1))
X24 <- with(d24, cbind(int = 1, seq = SEQ, prd = PRD, trt = TRT, car = CAR))
c(rank = qr(X24)$rank, columns = ncol(X24))
