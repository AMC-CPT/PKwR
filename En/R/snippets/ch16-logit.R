# Binary (yes/no) concentration-response: logistic regression equals sigmoid Emax.
set.seed(20260828)
EC50t <- 8; gmt <- 1.6                             # true values
Cd15 <- rep(c(0.5, 1, 2, 4, 8, 16, 32, 64), each = 40)
pr   <- Cd15^gmt/(EC50t^gmt + Cd15^gmt)            # sigmoid Emax as a probability
Yb   <- rbinom(length(Cd15), 1, pr)

# logit(p) = b0 + b1 log C is the same equation as p = C^b1/(exp(-b0/b1)^b1 + C^b1)
gl <- glm(Yb ~ log(Cd15), family = binomial)
b  <- coef(gl)
round(c(b0 = b[[1]], b1 = b[[2]], gamma.hat = b[[2]],
        EC50.hat = exp(-b[[1]]/b[[2]]), gamma.true = gmt, EC50.true = EC50t), 3)

par(mar = c(4.2, 4.8, 1.0, 1.0))
obs <- tapply(Yb, Cd15, mean)                      # observed response rate by conc.
plot(as.numeric(names(obs)), obs, log = "x", las = 1, bty = "l", pch = 16,
     ylim = c(0, 1), xlab = "Concentration (mg/L)", ylab = "Response probability")
cg <- 10^seq(-0.5, 2, 0.01)
lines(cg, plogis(b[[1]] + b[[2]]*log(cg)))
lines(cg, cg^gmt/(EC50t^gmt + cg^gmt), lty = 3)
abline(h = 0.5, lty = 3, col = "gray60")
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 3), pch = c(NA, NA),
       legend = c("logistic fit", "true sigmoid Emax"))
