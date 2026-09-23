# Three compartments: instead of 27 hand-written coefficients, code the Heaviside rule.
K13 <- 2; K31 <- 0.5
A3  <- matrix(c(-(K10 + K12 + K13), K21,  K31,
                  K12,             -K21,  0,
                  K13,              0,   -K31), 3, 3, byrow = TRUE)

# Denominator D(s) = s^3 + a2 s^2 + a1 s + a0: its three roots are -lambda (eq:lapD3).
a2   <- K10 + K12 + K13 + K21 + K31
a1   <- K10*K21 + K13*K21 + K10*K31 + K12*K31 + K21*K31
a0   <- K10*K21*K31
lam3 <- sort(-Re(polyroot(c(a0, a1, a2, 1))), decreasing = TRUE)

# adj(sI - A) = transpose of the cofactor matrix. Valid for any number of compartments.
adjM  <- function(M) outer(seq_len(nrow(M)), seq_len(nrow(M)),
           Vectorize(function(i, j) (-1)^(i + j)*det(M[-j, -i, drop = FALSE])))
Coefs <- function(A, L) lapply(seq_along(L), function(m)
           adjM(-L[m]*diag(nrow(A)) - A)/prod(L[-m] - L[m]))
Co3 <- Coefs(A3, lam3)

rbind(lambda = lam3, "half-life (hr)" = log(2)/lam3)
round(Co3[[1]], 4)                       # the nine coefficients of e^{-lambda_1 t}

# Check that the same rule also holds for two compartments and agrees with SolComp3 of
# wnl. The root order is not fixed, so the roots are matched by size before comparing.
M3  <- runN(dh2, lam3, Co3)
s3  <- SolComp3(K10, K12, K21, K13, K31)
ord <- order(s3$L, decreasing = TRUE)
c(rule.vs.2comp = max(abs(unlist(Coefs(Amt, lam)) - unlist(Co))),
  vs.SolComp3   = max(abs(unlist(Co3) - unlist(lapply(ord, function(m) s3$Co[, m, ])))),
  vs.nComp      = max(abs(M3 - nComp(s3, Ka, dh2))))
round(cbind(TIME = dh2$TIME, M3)[c(1, 2, 7, 10, 14, 15, 19), ], 4)
