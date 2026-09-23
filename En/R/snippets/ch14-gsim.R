# Several admission groups: for lack of beds, 28 subjects were admitted in 4 groups
# (10, 8, 6, 4). Baseline differences between groups (season, diet, etc.) are real,
# but in truth there is no group x formulation interaction.
set.seed(20260827)
nAdm  <- c(10, 8, 6, 4)
ADMs  <- rep(1:4, nAdm); nG <- sum(nAdm)
seqvG <- unlist(lapply(nAdm, function(k) rep(c("RT", "TR"), each = k/2)))
gd <- data.frame(SUBJ = rep(1:nG, each = 2), ADM = rep(ADMs, each = 2),
                 GRP  = rep(seqvG, each = 2), PRD = rep(1:2, nG))
gd$TRT <- ifelse((gd$GRP == "RT") == (gd$PRD == 1), "R", "T")

admEf <- c(0, 0.08, -0.12, 0.05)         # group baseline shifts (log scale, real)
iivG  <- rnorm(nG, 0, 0.34)
sWg   <- sqrt(log(1 + 0.22^2))           # within-subject CV 22%
gd$AUClast <- round(exp(log(1800) + admEf[gd$ADM] + iivG[gd$SUBJ] +
                    0.03*(gd$PRD == 2) + log(0.95)*(gd$TRT == "T") +
                    rnorm(2*nG, 0, sWg)), 1)

# Two period-2 dropouts (one in group 2, one in group 4): incomplete subjects
gd <- gd[!(gd$SUBJ %in% c(15, 27) & gd$PRD == 2), ]
with(unique(gd[, c("SUBJ", "ADM", "GRP")]), table(ADM, GRP))
c(n.subj = length(unique(gd$SUBJ)), n.rec = nrow(gd))
