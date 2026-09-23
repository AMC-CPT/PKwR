# Stage 2: enroll the re-estimated number of subjects and pool the two stages.
# The pooled model must include the Stage term: subjects are nested in stage x
# sequence, periods within stage. The decision again uses the 94.12% CI.
n2  <- n.new - n1
st2 <- data.frame(SUBJ = rep(n1 + 1:n2, each = 2), Stage = 2,
                  GRP  = rep(rep(c("RT", "TR"), each = n2/2), each = 2),
                  PRD  = rep(1:2, n2))
st2$TRT <- ifelse((st2$GRP == "RT") == (st2$PRD == 1), "R", "T")
st2$AUClast <- round(exp(log(1500) + rnorm(n2, 0, 0.3)[st2$SUBJ - n1] +
                     0.02*(st2$PRD == 2) + log(1.00)*(st2$TRT == "T") +
                     rnorm(2*n2, 0, sWt)), 1)

pool  <- af(rbind(st1, st2), c("SUBJ", "Stage", "GRP", "PRD", "TRT"))
f2  <- log(AUClast) ~ GRP + Stage:GRP:SUBJ + Stage/PRD + TRT
ci2 <- setNames(exp(CIest(f2, pool, "TRT", c(-1, 1),
                    conf.level = 1 - 2*a2)[1, 1:3]), c("PE", "LL", "UL"))
round(ci2, 4)
c(df.resid = GLM(f2, pool)$ANOVA["RESIDUALS", "Df"], n.total = n.new,
  BE = unname(ci2["LL"] >= 0.8 & ci2["UL"] <= 1.25))
