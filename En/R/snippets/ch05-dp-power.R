# Power model: log(AUC) = b0 + b1 log(Dose)
#   b1 = 1 means dose proportional.  Subject is a random effect (repeated measures).
library(nlme)
fit <- lme(log(AUC) ~ log(Dose), random = ~ 1 | Subject, data = pk)
b1  <- unname(fixef(fit)["log(Dose)"])
ci  <- unname(intervals(fit, which = "fixed")$fixed["log(Dose)",
                                                     c("lower", "upper")])
c(b1 = b1, lower = ci[1], upper = ci[2])
