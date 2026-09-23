# Weights: which errors are to be treated as 'equally important'
wsse <- function(p, w) { f <- p[1]*exp(-p[2]*d4$x); sum(w(f)*(d4$DV - f)^2) }
w.ols <- function(f) 1                        # equal weights (additive error)
w.wls <- function(f) 1/f^2                    # 1/f^2 weights (proportional error)
els   <- function(p) { f <- p[1]*exp(-p[2]*d4$x); v <- (p[3]*f)^2
                       sum(log(v) + (d4$DV - f)^2/v) }   # variance estimated as well

o1 <- optim(c(50, .1), wsse, w = w.ols, method = "L-BFGS-B",
            lower = c(1, .01), upper = c(1e3, 5))$par
o2 <- optim(c(50, .1), wsse, w = w.wls, method = "L-BFGS-B",
            lower = c(1, .01), upper = c(1e3, 5))$par
o3 <- optim(c(50, .1, .2), els, method = "L-BFGS-B",
            lower = c(1, .01, .01), upper = c(1e3, 5, 5))$par
r4 <- rbind(OLS = c(o1, NA), WLS = c(o2, NA), ELS = o3,
            true = c(A.t, k.t, 0.10))
colnames(r4) <- c("A", "k", "CV"); round(r4, 4)

# Why they differ: residuals side by side on the original and the relative scale
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (i in 1:2) {
  p <- if (i == 1) o1 else o2
  f <- p[1]*exp(-p[2]*d4$x)
  plot(f, (d4$DV - f)/f, las = 1, bty = "l", pch = 16, ylim = c(-0.3, 0.3),
       xlab = "Fitted value", ylab = "Relative residual",
       main = c("(a) OLS", "(b) WLS")[i], cex.main = 0.95)
  abline(h = 0, lty = 3)
}
