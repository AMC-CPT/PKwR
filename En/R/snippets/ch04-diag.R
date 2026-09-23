# The two signals residuals send: funnel (error model) and wave (structural model)
set.seed(20260828)                            # separate, truly two-exponential data
x2 <- c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 9, 12, 18, 24)
y2 <- round((70*exp(-1.2*x2) + 40*exp(-0.12*x2))*
            exp(rnorm(length(x2), 0, 0.07)), 3)
b1 <- optim(c(100, 0.3), function(p) sum((log(y2) - log(p[1]*exp(-p[2]*x2)))^2),
            method = "L-BFGS-B", lower = c(1, .01), upper = c(1e4, 20))$par

par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))
f.ols <- o1[1]*exp(-o1[2]*d4$x)
plot(f.ols, d4$DV - f.ols, las = 1, bty = "l", pch = 16, xlab = "Fitted value",
     ylab = "Residual", main = "(a) Unweighted, raw residuals"); abline(h = 0, lty = 3)
f.els <- o3[1]*exp(-o3[2]*d4$x)
plot(f.els, (d4$DV - f.els)/f.els, las = 1, bty = "l", pch = 16, ylim = c(-.3, .3),
     xlab = "Fitted value", ylab = "Relative residual",
     main = "(b) Proportional error, weighted residuals")
abline(h = 0, lty = 3)
f.mis <- b1[1]*exp(-b1[2]*x2)
plot(x2, (y2 - f.mis)/f.mis, las = 1, bty = "l", pch = 16,
     xlab = "x", ylab = "Relative residual", main = "(c) Model misspecification")
abline(h = 0, lty = 3); lines(x2, (y2 - f.mis)/f.mis, lty = 2)
