# Exposure data of one phase 1 dose group: log-normal AUC with 45% interindividual CV
# and Tmax tied to the sampling grid (observed only as 0.5, 1, 1.5, 2 or 3 hr)
n <- 24
AUC  <- rlnorm(n, meanlog = log(100), sdlog = sqrt(log(1 + 0.45^2)))
Tmax <- sample(c(0.5, 1, 1.5, 2, 3), n, replace = TRUE,
               prob = c(0.15, 0.35, 0.25, 0.15, 0.10))

round(c(arith.mean = mean(AUC), geo.mean = exp(mean(log(AUC))),
        median = median(AUC), SD = sd(AUC), SE = sd(AUC)/sqrt(n),
        CV.pct = 100*sd(AUC)/mean(AUC)), 1)
c(Tmax.median = median(Tmax), Tmax.min = min(Tmax), Tmax.max = max(Tmax))

hist(AUC, breaks = 12, col = "gray92", border = "gray35", main = "",
     las = 1, xlab = "AUC (mg*hr/L)")
abline(v = mean(AUC), lty = 1)
abline(v = exp(mean(log(AUC))), lty = 2)
abline(v = median(AUC), lty = 3)
legend("topright", bty = "n", cex = 0.85, lty = 1:3,
       legend = c("arithmetic mean", "geometric mean", "median"))
