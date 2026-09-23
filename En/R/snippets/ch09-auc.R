# Two trapezoidal rules: linear and log
auc.lin <- function(t, C) sum(diff(t)*(head(C, -1) + tail(C, -1))/2)
auc.ld  <- function(t, C) {                    # linear-up log-down
  C1 <- head(C, -1); C2 <- tail(C, -1)
  dn <- C2 < C1 & C2 > 0                       # log only on declining intervals
  sum(ifelse(dn, (C1 - C2)/log(C1/C2), (C1 + C2)/2)*diff(t))
}

# Take the noise-free true curve at the observation times only: bias of the method
Ct  <- C2iv(tobs)
ref <- integrate(C2iv, tobs[1], 48)$value      # true AUC (after the first sample)
round(c(true = ref, linear = auc.lin(tobs, Ct), lin.log = auc.ld(tobs, Ct)), 3)

# Apply to the observed (noisy) data
round(c(AUClast.lin = auc.lin(dat2$Time, dat2$DV),
        AUClast.ld  = auc.ld (dat2$Time, dat2$DV)), 3)
