# 3구획: 계수를 손으로 27개 적는 대신 Heaviside 규칙을 그대로 코드로 옮긴다.
K13 <- 2; K31 <- 0.5
A3  <- matrix(c(-(K10 + K12 + K13), K21,  K31,
                  K12,             -K21,  0,
                  K13,              0,   -K31), 3, 3, byrow = TRUE)

# 분모 D(s) = s^3 + a2 s^2 + a1 s + a0 의 세 근이 -lambda 이다 (식 (eq:lapD3)).
a2   <- K10 + K12 + K13 + K21 + K31
a1   <- K10*K21 + K13*K21 + K10*K31 + K12*K31 + K21*K31
a0   <- K10*K21*K31
lam3 <- sort(-Re(polyroot(c(a0, a1, a2, 1))), decreasing = TRUE)

# adj(sI - A) = 여인수행렬의 전치. 구획 수에 무관하다.
adjM  <- function(M) outer(seq_len(nrow(M)), seq_len(nrow(M)),
           Vectorize(function(i, j) (-1)^(i + j)*det(M[-j, -i, drop = FALSE])))
Coefs <- function(A, L) lapply(seq_along(L), function(m)
           adjM(-L[m]*diag(nrow(A)) - A)/prod(L[-m] - L[m]))
Co3 <- Coefs(A3, lam3)

rbind(lambda = lam3, "반감기(hr)" = log(2)/lam3)
round(Co3[[1]], 4)                       # e^{-lambda_1 t} 의 계수 아홉 개

# 같은 규칙이 2구획에서도 성립하는지, wnl 의 SolComp3 와 같은지 확인한다.
# 근의 순서는 정해진 것이 아니므로 크기순으로 맞추어 대조한다.
M3  <- runN(dh2, lam3, Co3)
s3  <- SolComp3(K10, K12, K21, K13, K31)
ord <- order(s3$L, decreasing = TRUE)
c(rule.vs.2comp = max(abs(unlist(Coefs(Amt, lam)) - unlist(Co))),
  vs.SolComp3   = max(abs(unlist(Co3) - unlist(lapply(ord, function(m) s3$Co[, m, ])))),
  vs.nComp      = max(abs(M3 - nComp(s3, Ka, dh2))))
round(cbind(TIME = dh2$TIME, M3)[c(1, 2, 7, 10, 14, 15, 19), ], 4)
