# Tmax is observed only at scheduled sampling times, so it is not continuous. With
# a true T-R difference of 0.35 h, simulate data and see how the discreteness shows.
set.seed(20260829)                      # reproducible [required]; subject-level draws
sch  <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 12, 24)   # sampling schedule
snap <- function(x) sch[sapply(x, function(v) which.min(abs(sch - v)))]
shft <- 0.35                            # truth: the test product's Tmax is later
dt   <- data.frame(SUBJ = rep(1:n, each = 2), SEQ = rep(seqv, each = 2),
                   PRD = rep(1:2, n))
dt$TRT  <- ifelse((dt$SEQ == "RT") == (dt$PRD == 1), "R", "T")
tsub    <- rnorm(n, 1.2, 0.45)          # subject differences in absorption rate
dt$Tmax <- snap(pmax(0.2, tsub[dt$SUBJ] + rnorm(2*n, 0, 0.25)) +
                ifelse(dt$TRT == "T", shft, 0))
table(dt$Tmax, dt$TRT)

# 48 observations fall on only 7 values; with so many ties, normality and equal
# variance are moot. The nonparametric crossover analysis compares individual
# 'period differences' between sequences (Hauschke et al.). The period effect is
# common to both and cancels; the remaining shift is twice the formulation effect.
tw  <- with(dt, tapply(Tmax, list(SUBJ, PRD), mean))
dif <- tw[, 1] - tw[, 2]
w   <- wilcox.test(dif ~ seqv, conf.int = TRUE, conf.level = 0.90,
                   exact = FALSE)
round(c(HL.est = -w$estimate[[1]]/2, lower = -w$conf.int[2]/2,
        upper = -w$conf.int[1]/2, p.value = w$p.value, true = shft), 4)

# What happens if the same data are treated parametrically (for reference)
round(c(t.est = -diff(rev(t.test(dif ~ seqv)$estimate))[[1]]/2,
        median.R = median(dt$Tmax[dt$TRT == "R"]),
        median.T = median(dt$Tmax[dt$TRT == "T"])), 4)
