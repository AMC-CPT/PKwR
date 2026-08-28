# 검정력 모형(power model): log(AUC) = b0 + b1 log(Dose)
#   b1 = 1 이면 용량 비례.  개인내 반복측정이므로 대상자를 확률효과로 둔다.
library(nlme)
fit <- lme(log(AUC) ~ log(Dose), random = ~ 1 | Subject, data = pk)
b1  <- unname(fixef(fit)["log(Dose)"])
ci  <- unname(intervals(fit, which = "fixed")$fixed["log(Dose)",
                                                     c("lower", "upper")])
c(b1 = b1, lower = ci[1], upper = ci[2])
