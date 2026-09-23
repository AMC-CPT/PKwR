# Obtain the two-compartment impulse response exp(At) three ways and check they agree.
K10 <- 0.1; K12 <- 3; K21 <- 1
Amt <- matrix(c(-(K10 + K12), K21,
                  K12,       -K21), 2, 2, byrow = TRUE)   # equation (eq:lapA2)

# (1) The two roots of the characteristic equation s^2 + (K10+K12+K21)s + K10*K21 = 0
ksum <- K10 + K12 + K21
lam <- c((ksum + sqrt(ksum^2 - 4*K10*K21))/2, (ksum - sqrt(ksum^2 - 4*K10*K21))/2)
c(lambda1 = lam[1], lambda2 = lam[2],          # equal to alpha and beta of Chapter 7
  check.sum = sum(lam) - ksum, check.prod = prod(lam) - K10*K21)

# (2) Heaviside cover-up: evaluate adj(sI - A) at s = -lambda_m and divide by the
#     differences from the other roots. 2x2 adj: swap diagonal, negate off-diagonal.
adjSIA <- function(s) matrix(c(s + K21, K21,
                               K12,     s + K10 + K12), 2, 2, byrow = TRUE)
Co    <- lapply(seq_along(lam), function(m) adjSIA(-lam[m])/prod(lam[-m] - lam[m]))
expAt <- function(tt) Reduce(`+`, Map(function(C, L) C*exp(-L*tt), Co, lam))

# (3) Matrix exponential from the eigendecomposition (numerical)
eig   <- eigen(Amt)
expAe <- function(tt) Re(eig$vectors %*% diag(exp(eig$values*tt)) %*%
                         solve(eig$vectors))

round(expAt(2), 6)
c(vs.eigen = max(abs(expAt(2) - expAe(2))),
  vs.wnl   = max(abs(unlist(Co) - aperm(SolComp2(K10, K12, K21)$Co, c(1, 3, 2)))))
