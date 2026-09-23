# Simulated 2x2 crossover BE data: period and formulation effects and within-subject
# error are added on the log scale on top of log-normal between-subject variation.
# Columns (SUBJ, GRP, PRD, TRT) follow the BE package; GRP is the sequence (RT/TR).
set.seed(20260827)                       # reproducible [required]; subject-level draws
n    <- 24                               # 12 subjects per sequence
seqv <- rep(c("RT", "TR"), each = n/2)   # sequence of each subject
d13  <- data.frame(SUBJ = rep(1:n, each = 2), GRP = rep(seqv, each = 2),
                   PRD  = rep(1:2, n))
d13$TRT <- ifelse((d13$GRP == "RT") == (d13$PRD == 1), "R", "T")

iiv <- rnorm(n, 0, 0.35)                 # between-subject SD 0.35 (CVb about 36%)
sWa <- sqrt(log(1 + 0.20^2))             # within-subject CV of AUC 20%
sWc <- sqrt(log(1 + 0.26^2))             # within-subject CV of Cmax 26%
gmr <- c(AUClast = 0.95, Cmax = 1.06)    # true GMR (T/R)
d13$AUClast <- round(exp(log(2000) + iiv[d13$SUBJ] + 0.03*(d13$PRD == 2) +
                     log(gmr["AUClast"])*(d13$TRT == "T") +
                     rnorm(2*n, 0, sWa)), 1)
d13$Cmax    <- round(exp(log(480) + iiv[d13$SUBJ] + 0.02*(d13$PRD == 2) +
                     log(gmr["Cmax"])*(d13$TRT == "T") +
                     rnorm(2*n, 0, sWc)), 1)
head(d13, 4)

# A naive summary without a model: geometric means by formulation and their ratio
lg <- aggregate(cbind(AUClast = log(AUClast), Cmax = log(Cmax)) ~ TRT,
                d13, mean)
gm <- exp(as.matrix(lg[, -1])); rownames(gm) <- lg$TRT
round(gm, 1)
round(gm["T", ]/gm["R", ], 4)
