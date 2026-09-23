# What if one more exponential term is added? The parameters grow from two to four
sse2 <- function(p) sum((d4$DV - p[1]*exp(-p[2]*d4$x) - p[3]*exp(-p[4]*d4$x))^2)
st2 <- rbind(c(50, 1.0, 50, 0.2), c(80, 0.3, 20, 0.05), c(10, 2.0, 90, 0.3))
f2  <- t(apply(st2, 1, function(s) {
  o <- optim(s, sse2, method = "L-BFGS-B", lower = rep(1e-4, 4),
             upper = c(1e4, 20, 1e4, 20))
  c(o$par, SSE = o$value) }))
colnames(f2) <- c("A1", "k1", "A2", "k2", "SSE")
round(cbind(f2, A1plusA2 = f2[, "A1"] + f2[, "A2"]), 3)

# Model selection: SSE always decreases as parameters are added. AIC imposes a penalty
n <- nrow(d4)
aic <- function(sse, p) n*log(sse/n) + 2*p
round(c(SSE.1exp = fit.ols$value, SSE.2exp = min(f2[, "SSE"]),
        AIC.1exp = aic(fit.ols$value, 3), AIC.2exp = aic(min(f2[, "SSE"]), 5)), 3)
