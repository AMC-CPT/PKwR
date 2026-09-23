# Step size h: truncation error if large, round-off if small; the optimum is in between
truth <- -k*A*exp(-k*4)                       # true value of g'(4)
hs  <- 10^seq(-1, -14, by = -0.25)
fwd <- abs(sapply(hs, function(h) (g(4 + h) - g(4))/h) - truth)          # forward
ctr <- abs(sapply(hs, function(h) (g(4 + h) - g(4 - h))/(2*h)) - truth)  # central
signif(c(best.h.fwd = hs[which.min(fwd)], sqrt.eps = sqrt(.Machine$double.eps),
         best.h.ctr = hs[which.min(ctr)], cube.root.eps = .Machine$double.eps^(1/3),
         min.err.fwd = min(fwd), min.err.ctr = min(ctr)), 3)

par(mar = c(4.2, 6.4, 1.0, 1.0))
plot(hs, fwd, log = "xy", type = "l", las = 1, bty = "l", ylim = c(1e-12, 1e-1),
     xlab = "h", ylab = "")
title(ylab = "absolute error", line = 5)      # axis label outside the tick labels
lines(hs, ctr, lty = 2)
abline(v = c(sqrt(.Machine$double.eps), .Machine$double.eps^(1/3)), lty = 3,
       col = "gray55")
legend("bottomleft", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("forward difference (error ~ h)", "central difference (error ~ h^2)"))
