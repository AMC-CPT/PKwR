# Critical region of Smith et al. (2000): for the dose ratio r = Dmax/Dmin, if
#   1 + log(0.8)/log(r)  <=  b1  <=  1 + log(1.25)/log(r)
# the dose-normalized exposure is taken to lie within (0.8, 1.25): proportional.
crit <- function(r, theta = 1.25)
  unname(c(1 - log(theta)/log(r), 1 + log(theta)/log(r)))

r <- max(pk$Dose)/min(pk$Dose)
cr <- crit(r)
c(dose.ratio = r, crit.lower = cr[1], crit.upper = cr[2],
  b1.lower = ci[1], b1.upper = ci[2],
  proportional = as.numeric(ci[1] >= cr[1] & ci[2] <= cr[2]))
