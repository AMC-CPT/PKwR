# Standard 2x2 analysis: log(Y) ~ sequence + subject(sequence) + period + formulation.
# sasLM gives the same results as SAS PROC GLM (the author's package).
library(sasLM)
d13f <- af(d13, c("SUBJ", "GRP", "PRD", "TRT"))       # factorize
f1 <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
GLM(f1, d13f)$`Type III`

# Sequence (GRP) alone must be tested against the 'subject' term, not the residual
RanTest(f1, d13f, Random = "SUBJ")$GRP

# 90% confidence interval of the GMR: estimate the T - R log difference, exponentiate
ci <- setNames(exp(CIest(f1, d13f, "TRT", c(-1, 1),
                         conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL"))
mse <- GLM(f1, d13f)$ANOVA["RESIDUALS", "Mean Sq"]
round(c(ci, CVw.pct = BE::mse2cv(mse)), 4)
c(BE = unname(ci["LL"] >= 0.8 & ci["UL"] <= 1.25))

# Cmax with the same model
round(setNames(exp(CIest(log(Cmax) ~ GRP/SUBJ + PRD + TRT, d13f, "TRT",
      c(-1, 1), conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL")), 4)
