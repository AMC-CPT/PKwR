# 한 사람의 정보행렬(FO 근사, 고정효과 블록). PRED 가 이미 dF/deta 를 준다.
# theta_k*exp(eta_k) 모수화이므로 eta=0 에서 dF/dtheta_k = (dF/deta_k)/theta_k.
FIM <- function(tt, dose = 320) {
  M  <- PRED(TH, c(0, 0, 0), cbind(TIME = tt, DOSE = dose))
  G  <- M[, c("G1", "G2", "G3"), drop = FALSE]                    # dF/deta
  V  <- G %*% OM %*% t(G) + diag(SG[1,1]*M[, "F"]^2 + SG[2,2], nrow = length(tt))
  dF <- sweep(G, 2, TH, "/")                                      # dF/dtheta
  t(dF) %*% solve(V) %*% dF
}
sef <- function(M, N = 12)                          # N 명일 때의 표준오차 예측
  sqrt(diag(solve(N*M, tol = 1e-30)))               # 특이에 가까워도 그대로 본다

# 검산: 실제 설계(11점, 12명)로 예측한 SE 와 CovStep 이 준 SE
tgrid <- c(0.25, 0.5, 1, 2, 3.5, 5, 7, 9, 12, 18, 24)
round(rbind(FIM.pred = sef(FIM(tgrid)), CovStep = SE[1:3]), 4)

# 3점 설계를 전수 탐색한다 (D-최적 = det(M) 최대)
cand  <- combn(tgrid, 3)
dets  <- apply(cand, 2, function(z) det(FIM(z)))
opt   <- cand[, which.max(dets)]; wst <- cand[, which.min(dets)]
naive <- c(1, 5, 24)
rbind(optimal = opt, naive = naive, worst = wst)

round(rbind(rich11   = sef(FIM(tgrid)),
            optimal3 = sef(FIM(opt)),
            naive3   = sef(FIM(naive)),
            worst3   = sef(FIM(wst))), 4)

# D-효율: 11점 설계와 같은 정보를 얻으려면 사람이 몇 배 필요한가
dr <- det(FIM(tgrid))
eff <- c(optimal3 = max(dets), naive3 = det(FIM(naive)), worst3 = min(dets))/dr
round(rbind(D.eff = eff^(1/3), N.ratio = eff^(-1/3)), 4)
