# (1) Full diagnostic model: group, sequence, subject in group x seq, period in group,
#     formulation, group x formulation. Only the interaction test is read (FDA 2026).
gdf <- af(gd, c("SUBJ", "ADM", "GRP", "PRD", "TRT"))
ff  <- log(AUClast) ~ ADM + GRP + ADM:GRP:SUBJ + ADM:PRD + TRT + ADM:TRT
GLM(ff, gdf)$`Type III`

# (2) Primary: standard model, no group or interaction; GLM with complete subjects
fr  <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
ciG <- setNames(exp(CIest(fr, gdf, "TRT", c(-1, 1),
                    conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL"))
round(ciG, 4)

# (3) Mixed-effects model (REML): uses all data, including dropouts' period 1
library(nlme)
lmeG <- lme(log(AUClast) ~ GRP + PRD + TRT, random = ~1 | SUBJ, data = gdf)
round(exp(intervals(lmeG, 0.90)$fixed["TRTT", ]), 4)
c(n.GLM = sum(table(gdf$SUBJ) == 2), n.lme = length(unique(gdf$SUBJ)))
