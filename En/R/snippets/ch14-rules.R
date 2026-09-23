# Apply one abandoned rule directly. The 75/75 rule required at least 75% of the
# individual T/R ratios to lie within (0.75, 1.25).
rr    <- with(d13, tapply(AUClast, list(SUBJ, TRT), mean))
ratio <- rr[, "T"]/rr[, "R"]
round(c(n = length(ratio), within = mean(ratio > 0.75 & ratio < 1.25),
        pass = mean(ratio > 0.75 & ratio < 1.25) >= 0.75), 3)

# Where this rule breaks down is clear. Even if the formulations are 'identical'
# (GMR = 1), each ratio carries the within-subject error twice: log SD = sqrt(2)*sw.
# So the expected fraction within the range can be written in closed form.
frac <- function(cv) { s <- sqrt(2*log(1 + cv^2))
                       pnorm(log(1.25)/s) - pnorm(log(0.75)/s) }
cw <- c(0.10, 0.15, 0.20, 0.25, 0.30)
round(rbind(CVw = cw, expected.within = frac(cw), rule.needs = 0.75), 3)

# 80/125 is symmetric on the log scale; the (+-20%) rule is not
round(c(lo = log(0.80), hi = log(1.25), sum = log(0.80) + log(1.25),
        pm20.hi = log(1.20), pm20.sum = log(0.80) + log(1.20)), 4)
