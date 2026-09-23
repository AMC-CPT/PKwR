# Dosing history: 100 mg IV bolus at 0 h, 150 mg infused at 50 mg/hr from 24 h, and
# 100 mg oral at 48 h. CMT 1 = absorption site (gut), CMT 2 = central compartment.
library(wnl)
dh <- data.frame(
  TIME = c(0, 1, 2, 4, 8, 12, 24, 25, 26, 28, 32, 36, 48, 49, 50, 52, 56, 60),
  AMT  = c(100, rep(NA, 5), 150, rep(NA, 5), 100, rep(NA, 5)),
  RATE = c(  0, rep(NA, 5),  50, rep(NA, 5),   0, rep(NA, 5)),
  CMT  = c(  2, rep(NA, 5),   2, rep(NA, 5),   1, rep(NA, 5)))

# The analytic solution holds as one formula only where the input is constant. Hour 27
# (end of infusion) is unobserved, but the derivative breaks there: add it to the grid.
dh2 <- ExpandDH(dh)
dh2[, c("TIME", "AMT", "RATE", "CMT", "BOLUS", "RATE2")]
