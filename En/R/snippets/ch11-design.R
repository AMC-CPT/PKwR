# One subject's information matrix (FO approximation, fixed-effect block). PRED gives
# dF/deta; with theta_k*exp(eta_k), at eta=0 dF/dtheta_k = (dF/deta_k)/theta_k.
FIM <- function(tt, dose = 320) {
  M  <- PRED(TH, c(0, 0, 0), cbind(TIME = tt, DOSE = dose))
  G  <- M[, c("G1", "G2", "G3"), drop = FALSE]                    # dF/deta
  V  <- G %*% OM %*% t(G) + diag(SG[1,1]*M[, "F"]^2 + SG[2,2], nrow = length(tt))
  dF <- sweep(G, 2, TH, "/")                                      # dF/dtheta
  t(dF) %*% solve(V) %*% dF
}
sef <- function(M, N = 12)                          # predicted SE with N subjects
  sqrt(diag(solve(N*M, tol = 1e-30)))               # proceed even when nearly singular

# Cross-check: SE predicted for the actual design (11 points, 12 subjects) vs CovStep
tgrid <- c(0.25, 0.5, 1, 2, 3.5, 5, 7, 9, 12, 18, 24)
round(rbind(FIM.pred = sef(FIM(tgrid)), CovStep = SE[1:3]), 4)

# Exhaustive search of the three-point designs (D-optimal = maximum det(M))
cand  <- combn(tgrid, 3)
dets  <- apply(cand, 2, function(z) det(FIM(z)))
opt   <- cand[, which.max(dets)]; wst <- cand[, which.min(dets)]
naive <- c(1, 5, 24)
rbind(optimal = opt, naive = naive, worst = wst)

round(rbind(rich11   = sef(FIM(tgrid)),
            optimal3 = sef(FIM(opt)),
            naive3   = sef(FIM(naive)),
            worst3   = sef(FIM(wst))), 4)

# D-efficiency: how many times more subjects for the same information as 11 points
dr <- det(FIM(tgrid))
eff <- c(optimal3 = max(dets), naive3 = det(FIM(naive)), worst3 = min(dets))/dr
round(rbind(D.eff = eff^(1/3), N.ratio = eff^(-1/3)), 4)
