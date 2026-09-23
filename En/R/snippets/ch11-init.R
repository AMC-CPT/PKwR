library(nmw)
e <- get("e", envir = asNamespace("nmw"))   # expose the internal environment PRED uses

THETAinit <- c(2, 50, 0.1)                            # initial estimates of KA, V, K
OMinit <- matrix(c(0.2, 0.1, 0.1,                     # full block OMEGA
                   0.1, 0.2, 0.1,
                   0.1, 0.1, 0.2), nrow = 3)
SGinit <- diag(c(0.1, 0.1))                           # proportional, additive
InitStep(DATA, THETAinit = THETAinit, OMinit = OMinit, SGinit = SGinit,
         LB = rep(0, 3), UB = rep(1e6, 3), Pred = PRED, METHOD = "ZERO")
