# Reparameterization: same model, new coordinates. SSE is unchanged, properties differ.
t0 <- sum(d4$x*exp(-2*pe4[2]*d4$x))/sum(exp(-2*pe4[2]*d4$x))   # data's center of mass
mods <- list(
  "A, k"     = list(f = function(q) sum((d4$DV - q[1]*exp(-q[2]*d4$x))^2),
                    ini = c(100, 0.25)),
  "A, log k" = list(f = function(q) sum((d4$DV - q[1]*exp(-exp(q[2])*d4$x))^2),
                    ini = c(100, log(0.25))),
  "f(t0), k" = list(f = function(q) sum((d4$DV - q[1]*exp(-q[2]*(d4$x - t0)))^2),
                    ini = c(30, 0.25)))
fits <- lapply(mods, function(m) optim(m$ini, m$f, method = "BFGS", hessian = TRUE))
res <- t(sapply(fits, function(o) c(par1 = o$par[1], par2 = o$par[2],
  SSE = o$value, corr = cov2cor(solve(o$hessian))[1, 2])))
round(c(t0 = t0), 3); round(res, 4)

# Profile (likelihood) intervals are transformation-invariant; Wald intervals are not
ci.k <- range((pe4[2] + se4[2]*dg)[abs(tK) <= tc])
se.lk <- sqrt(diag(2*s2*solve(fits[["A, log k"]]$hessian)))[2]
round(rbind(Wald.on.k     = pe4[2] + c(-1, 1)*tc*se4[2],
            Wald.on.log.k = exp(fits[["A, log k"]]$par[2] + c(-1, 1)*tc*se.lk),
            profile       = ci.k), 4)
asym <- function(ci, est) (ci[2] - est)/(est - ci[1])       # upper/lower width ratio
round(c(asym.k = asym(ci.k, pe4[2]), asym.log.k = asym(log(ci.k), log(pe4[2]))), 4)
