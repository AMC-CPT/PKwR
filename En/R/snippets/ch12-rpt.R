# RPT: permute the covariate (BWT) among subjects to build the dOFV distribution of a
# "world with no effect". Fast FO is used to save time (in practice, the final method).
# Reproducibility: seed set; resampling unit = subject; permutation, no replacement.
fo.ofv <- function(D, PredF, IE, LB, UB) {
  InitStep(D, THETAinit = IE, OMinit = OMinit, SGinit = SGinit,
           LB = LB, UB = UB, Pred = PredF, METHOD = "ZERO")
  EstStep()$Optim$value
}
IE4 <- c(3.2, 38, 0.11, 0.5)                       # FO-based initial values
ofv.full <- fo.ofv(DATA, PRED4, IE4, c(0, 0, 0, -5), c(1e6, 1e6, 1e6, 5))
dOFV.obs <- r.fo$Optim$value - ofv.full            # observed dOFV (FO basis)

uid <- unique(DATA$ID)
wt0 <- DATA$BWT[match(uid, DATA$ID)]
set.seed(20260827)
null <- replicate(50, {                            # 1000 or more in practice
  Dp <- DATA
  Dp$BWT <- sample(wt0)[match(Dp$ID, uid)]         # match: preserves row order
  stopifnot(all(tapply(Dp$BWT, Dp$ID,              # assertion after permutation:
                       function(x) length(unique(x))) == 1),
            all(sort(as.numeric(tapply(Dp$BWT, Dp$ID, unique))) == sort(wt0)))
  r.fo$Optim$value -
    fo.ofv(Dp, PRED4, IE4, c(0, 0, 0, -5), c(1e6, 1e6, 1e6, 5))
})
c(dOFV.obs = round(dOFV.obs, 2),
  p.perm = round((1 + sum(null >= dOFV.obs))/(1 + length(null)), 3),
  dOFV.crit = round(quantile(null, 0.95)[[1]], 2))

hist(null, breaks = 12, las = 1, col = "gray92", main = "",
     xlab = expression(Delta*"OFV under permutation"),
     xlim = range(c(null, dOFV.obs)))
abline(v = dOFV.obs, lwd = 1.4)
text(dOFV.obs, par("usr")[4]*0.9, "observed", pos = 2, cex = 0.85)
