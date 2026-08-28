library(nmw)
e <- get("e", envir = asNamespace("nmw"))   # PRED 가 참조할 내부 환경을 노출

THETAinit <- c(2, 50, 0.1)                            # KA, V, K 초기값
OMinit <- matrix(c(0.2, 0.1, 0.1,                     # full block OMEGA
                   0.1, 0.2, 0.1,
                   0.1, 0.1, 0.2), nrow = 3)
SGinit <- diag(c(0.1, 0.1))                           # 비례, 가법
InitStep(DATA, THETAinit = THETAinit, OMinit = OMinit, SGinit = SGinit,
         LB = rep(0, 3), UB = rep(1e6, 3), Pred = PRED, METHOD = "ZERO")
