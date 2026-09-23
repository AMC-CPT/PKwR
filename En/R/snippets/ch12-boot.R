# Bootstrap: resample subjects with replacement; draw by ID value and assign new IDs.
# Reproducibility: set seed. Bounds as wide as possible; count and report failed fits.
set.seed(20260827)
B <- 100                                           # 1000 or more in practice
bt <- matrix(NA_real_, 3, B, dimnames = list(c("KA", "V", "K"), NULL))
for (b in 1:B) {
  pick <- sample(uid, replace = TRUE)
  Db <- do.call(rbind, lapply(seq_along(pick), function(i) {
    d <- DATA[DATA$ID == pick[i], ]                # drawn by ID "value"
    d$ID <- i                                      # new ID for a duplicated subject
    d
  }))
  stopifnot(length(unique(Db$ID)) == length(uid))  # assertion: subject count preserved
  fit <- try(silent = TRUE, {
    InitStep(Db, THETAinit = c(3.2, 38, 0.11), OMinit = OMinit,
             SGinit = SGinit, LB = rep(0, 3), UB = rep(1e6, 3),
             Pred = PRED, METHOD = "ZERO")
    EstStep()[["Final Estimates"]][1:3]
  })
  if (!inherits(fit, "try-error")) bt[, b] <- fit
}
ok <- !is.na(bt[1, ])
c(n.success = sum(ok), n.fail = sum(!ok))

ci <- apply(bt[, ok], 1, quantile, c(0.025, 0.5, 0.975))
round(rbind(ci, asymptotic.SE = cov.fo[["Standard Error"]][1:3]), 4)
