# 2구획의 임펄스 응답 exp(At) 를 세 가지 방법으로 구해 같은지 본다.
K10 <- 0.1; K12 <- 3; K21 <- 1
Amt <- matrix(c(-(K10 + K12), K21,
                  K12,       -K21), 2, 2, byrow = TRUE)   # 식 (eq:lapA2)

# (1) 특성방정식 s^2 + (K10+K12+K21)s + K10*K21 = 0 의 두 근
ksum <- K10 + K12 + K21
lam <- c((ksum + sqrt(ksum^2 - 4*K10*K21))/2, (ksum - sqrt(ksum^2 - 4*K10*K21))/2)
c(lambda1 = lam[1], lambda2 = lam[2],          # 7장의 alpha, beta 와 같은 수이다
  check.sum = sum(lam) - ksum, check.prod = prod(lam) - K10*K21)

# (2) Heaviside 덮기: adj(sI - A) 를 s = -lambda_m 에서 값매김하고 나머지 근과의
#     차로 나눈다. 2x2 의 adj 는 대각을 맞바꾸고 비대각의 부호를 바꾼 것이다.
adjSIA <- function(s) matrix(c(s + K21, K21,
                               K12,     s + K10 + K12), 2, 2, byrow = TRUE)
Co    <- lapply(seq_along(lam), function(m) adjSIA(-lam[m])/prod(lam[-m] - lam[m]))
expAt <- function(tt) Reduce(`+`, Map(function(C, L) C*exp(-L*tt), Co, lam))

# (3) 고유값 분해로 얻은 행렬 지수 (수치해)
eig   <- eigen(Amt)
expAe <- function(tt) Re(eig$vectors %*% diag(exp(eig$values*tt)) %*% solve(eig$vectors))

round(expAt(2), 6)
c(vs.eigen = max(abs(expAt(2) - expAe(2))),
  vs.wnl   = max(abs(unlist(Co) - aperm(SolComp2(K10, K12, K21)$Co, c(1, 3, 2)))))
