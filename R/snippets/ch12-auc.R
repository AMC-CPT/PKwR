# 목표가 골농도가 아니라 AUC 인 경우(vancomycin). AUC24 = D_daily/CL 이므로
# 필요한 것은 개인 CL 하나다. 항정상태 두 점과 골 한 점을 비교한다.
mapSS <- function(tobs, yobs) optim(c(0, 0, 0), function(eta) {
    f <- css(eta, tobs, D.new, 12); v <- f^2*SG[["prop"]] + SG[["add"]]
    sum(log(v) + (yobs - f)^2/v) + drop(t(eta) %*% iOM %*% eta) },
  method = "BFGS", hessian = TRUE)

set.seed(20260828)
t.ss <- c(2, 11.5)                                  # 피크 부근과 골
y.ss <- round(css(eta.true, t.ss, D.new, 12)*(1 + rnorm(2, 0, sqrt(SG[["prop"]]))) +
              rnorm(2, 0, sqrt(SG[["add"]])), 2)
rbind(TIME = t.ss, DV = y.ss)

aucci <- function(o) {                              # AUC24 와 그 사후 구간
  cl <- TH[[2]]*exp(o$par[2])*TH[[3]]*exp(o$par[3])
  s  <- sqrt(sum((2*solve(o$hessian))[2:3, 2:3]))   # log CL 의 사후 SD
  a  <- 2*D.new/cl
  c(CL = cl, AUC24 = a, LL = a*exp(-1.96*s), UL = a*exp(1.96*s)) }
cl.t <- TH[[2]]*exp(eta.true[2])*TH[[3]]*exp(eta.true[3])
round(rbind(two.points = aucci(mapSS(t.ss, y.ss)),
            trough.only = aucci(mapSS(t.ss[2], y.ss[2])),
            prior.only = aucci(mapSS(numeric(0), numeric(0))),
            truth = c(cl.t, 2*D.new/cl.t, NA, NA)), 2)
