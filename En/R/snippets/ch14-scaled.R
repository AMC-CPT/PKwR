# Reference scaling for highly variable drugs: the BE limits widen in proportion
# to the reference within-subject variability (CVwR). Computed with scaledBound of
# the BE package (EMA ABEL: k = 0.76, expansion capped at CVwR 50%). CV is in %.
cvs2 <- seq(30, 60, 5)
sb <- t(sapply(cvs2, function(cv) scaledBound(CV = cv)))
dimnames(sb) <- list(paste0("CVwR ", cvs2, "%"), c("lower", "upper"))
round(sb, 4)

plot(cvs2, sb[, "upper"], type = "b", pch = 16, las = 1, bty = "l",
     ylim = c(0.6, 1.5), xlab = "CVwR (%)", ylab = "BE limits",
     panel.first = abline(h = c(0.8, 1, 1.25), lty = c(3, 2, 3)))
lines(cvs2, sb[, "lower"], type = "b", pch = 16)
text(58, c(sb[7, "lower"], sb[7, "upper"]), c("lower", "upper"),
     cex = 0.8, pos = c(1, 3))
