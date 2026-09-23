# Objective function: given parameters, returns 'how badly the model misses the data'
sse <- function(p) sum((d4$DV - p[1]*exp(-p[2]*d4$x))^2)
sse(c(100, 0.25)); sse(c(80, 0.25))          # smaller near the true values

# The optimizer finds the p that minimizes this function
fit.ols <- optim(c(50, 0.1), sse, method = "L-BFGS-B",
                 lower = c(1, 0.01), upper = c(1e3, 5))
round(c(A = fit.ols$par[1], k = fit.ols$par[2], SSE = fit.ols$value,
        n.eval = fit.ols$counts[["function"]]), 4)

# Look at the terrain of the objective function: the valley curves like a banana
Ag <- seq(60, 150, length.out = 80); kg <- seq(0.15, 0.40, length.out = 80)
Z  <- outer(Ag, kg, Vectorize(function(a, b) sse(c(a, b))))
contour(Ag, kg, log(Z), las = 1, bty = "l", nlevels = 18, drawlabels = FALSE,
        xlab = "A", ylab = "k", main = "Contours of log SSE")
points(A.t, k.t, pch = 3, cex = 1.3); points(fit.ols$par[1], fit.ols$par[2], pch = 16)
legend("topright", bg = "white", box.col = "gray70", cex = 0.8, pch = c(3, 16),
       legend = c("true value", "estimate"))
