# Two-stage sequential design (Potvin Method C): start with 12 subjects in stage 1.
# The plan assumed CV 20%, but what if the actual within-subject CV is larger?
set.seed(20260827)
n1  <- 12
st1 <- data.frame(SUBJ = rep(1:n1, each = 2), Stage = 1,
                  GRP  = rep(rep(c("RT", "TR"), each = n1/2), each = 2),
                  PRD  = rep(1:2, n1))
st1$TRT <- ifelse((st1$GRP == "RT") == (st1$PRD == 1), "R", "T")
sWt <- sqrt(log(1 + 0.26^2))              # actual CV_w 26% (true GMR is 1.0)
st1$AUClast <- round(exp(log(1500) + rnorm(n1, 0, 0.3)[st1$SUBJ] +
                     0.02*(st1$PRD == 2) + log(1.00)*(st1$TRT == "T") +
                     rnorm(2*n1, 0, sWt)), 1)
st1f <- af(st1, c("SUBJ", "GRP", "PRD", "TRT"))

# Stage-1 power (at alpha = 0.05) is below 80%, so decide with the adjusted alpha
a2 <- 0.0294                              # nominal significance level, Pocock K = 2
f1 <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
mse1 <- GLM(f1, st1f)$ANOVA["RESIDUALS", "Mean Sq"]
CV1  <- mse2cv(mse1)/100
library(PowerTOST)
pw1 <- power.TOST(CV = CV1, theta0 = 0.95, n = n1, method = "central")
ci1 <- setNames(exp(CIest(f1, st1f, "TRT", c(-1, 1),
                    conf.level = 1 - 2*a2)[1, 1:3]), c("PE", "LL", "UL"))
round(c(CV1 = CV1, power.at.0.05 = pw1), 4)
round(ci1, 4)                             # 94.12% CI: falls outside 80-125

# Re-estimate the sample size from the observed CV (adjusted alpha, planned GMR 0.95)
n.new <- 2*ssmse(mse1, True.R = 0.95, Alpha = 2*a2)
c(n.new = n.new, n2 = n.new - n1)
