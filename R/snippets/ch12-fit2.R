# MAP 추정기: 목적함수는 12.3절과 같고, 실무용으로 세 가지를 덧댄다.
OMv <- diag(c(0.149, 0.12, 1e-6, 0.416))          # V2 는 IIV 가 없어 0 에 고정
SGv <- c(prop = 0.231^2, add = 0)                 # 순수 비례오차
objv <- function(eta, Dv, THv, iOMv, CLcr) {
  f  <- pred2c(THv, eta, Dv, CLcr); ok <- !is.na(Dv$DV)
  v  <- f[ok]^2*SGv[["prop"]] + SGv[["add"]]
  kp <- is.finite(v) & v > 0                      # 예측 0 인 점은 정보가 없다
  if (!any(kp)) return(drop(t(eta) %*% iOMv %*% eta))
  sum(log(v[kp]) + (Dv$DV[ok][kp] - f[ok][kp])^2/v[kp]) + drop(t(eta) %*% iOMv %*% eta) }

fitEBE <- function(Dv, THv, OMv, CLcr) {          # 다중 출발점 MAP
  iOMv <- solve(OMv); s0 <- sqrt(diag(OMv)); nE <- nrow(OMv)
  st <- list(rep(0, nE))
  for (k in 1:nE) for (z in c(-1.5, 1.5)) {
    v <- rep(0, nE); v[k] <- z*s0[k]; st <- c(st, list(v)) }
  best <- NULL
  for (s in st) {
    r <- optim(optim(s, objv, Dv = Dv, THv = THv, iOMv = iOMv, CLcr = CLcr,
                     method = "Nelder-Mead")$par,
               objv, Dv = Dv, THv = THv, iOMv = iOMv, CLcr = CLcr, method = "BFGS")
    if (is.null(best) || r$value < best$value) best <- r }
  ok <- !is.na(Dv$DV)
  G  <- numDeriv::jacobian(function(e) pred2c(THv, e, Dv, CLcr)[ok], best$par)
  V  <- pred2c(THv, best$par, Dv, CLcr)[ok]^2*SGv[["prop"]] + SGv[["add"]]
  list(eta = best$par, OFV = best$value,          # 사후 공분산은 FOCE 정보행렬로
       COV = solve(t(G) %*% (G/V) + iOMv)) }
