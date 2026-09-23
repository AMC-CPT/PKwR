# How to handle observations below the limit of quantification (BLQ). Simulate data
# sampled out to 48 h and censor them at LLOQ = 1 mg/L. (The pooled fit absorbs the
# IIV, so the distance measured here is from the 'uncensored fit', not from truth.)
set.seed(20260828)
lloq <- 1.0
tt   <- c(0.25, 0.5, 1, 2, 4, 7, 9, 12, 24, 36, 48)
sim  <- do.call(rbind, lapply(1:12, function(i) {
  et <- drop(t(chol(OM)) %*% rnorm(3))            # individual deviations under OMEGA
  f  <- PRED(TH, et, cbind(TIME = tt, DOSE = 320))[, "F"]
  data.frame(ID = i, TIME = tt, DOSE = 320,
             DV = f + f*rnorm(length(tt), 0, sqrt(SG[1,1])) +
                      rnorm(length(tt), 0, sqrt(SG[2,2])))
}))
c(n = nrow(sim), n.blq = sum(sim$DV < lloq),
  pct.blq = round(100*mean(sim$DV < lloq), 1))
round(tapply(sim$DV < lloq, sim$TIME, mean), 3)    # BLQ fraction by time

# The three treatments in one objective function. Only two lines differ.
Fpop <- function(th, dose, t)
  dose/th[2]*th[1]/(th[1] - th[3])*(exp(-th[3]*t) - exp(-th[1]*t))

m2ll <- function(p, d, how) {
  th <- exp(p[1:3]); sg <- exp(p[4:5])
  f  <- Fpop(th, d$DOSE, d$TIME); v <- sg[1]*f^2 + sg[2]
  y  <- d$DV; b <- y < lloq                        # censored observations
  if (how == "full") b[] <- FALSE                  # uncensored data (reference)
  if (how == "M5") { y[b] <- lloq/2; b[] <- FALSE } # substitute LLOQ/2
  o <- sum(log(v[!b]) + (y[!b] - f[!b])^2/v[!b])   # M1 stops here (discards them)
  if (how == "M3" && any(b))                       # M3: add P(Y < LLOQ)
    o <- o - 2*sum(pnorm(lloq, f[b], sqrt(v[b]), log.p = TRUE))
  o
}
p0  <- log(c(TH, SG[1,1], SG[2,2]))
fit <- function(how) exp(optim(p0, m2ll, d = sim, how = how,
                               control = list(maxit = 3000, reltol = 1e-12))$par)
res <- sapply(c("full", "M1", "M5", "M3"), fit)
rownames(res) <- c("ka", "V", "ke", "sg.prop", "sg.add")
res <- rbind(res, CL = res["V", ]*res["ke", ], t.half = log(2)/res["ke", ])
round(res, 4)
round(100*(res[, c("M1", "M5", "M3")]/res[, "full"] - 1), 2)   # bias % vs. full

# Figure: discarding the tail or filling it with a constant flattens the terminal slope
tg <- seq(0.05, 48, 0.05)
plot(sim$TIME, pmax(sim$DV, 0.06), log = "y", las = 1, bty = "n", cex = 0.6,
     pch = ifelse(sim$DV < lloq, 1, 16),
     col = ifelse(sim$DV < lloq, "grey60", "grey25"),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)", ylim = c(0.06, 20))
abline(h = lloq, lty = 3)
text(46, lloq*1.25, "LLOQ", cex = 0.8, adj = 1)
for (j in seq_len(4)) lines(tg, Fpop(res[1:3, j], 320, tg), lwd = 2,
                            lty = c(1, 2, 4, 1)[j], col = c(1, 2, 4, 3)[j])
legend("bottomleft", bty = "n", cex = 0.85, lwd = 2, lty = c(1, 2, 4, 1),
       col = c(1, 2, 4, 3), legend = c("Uncensored data", "M1 (discard)",
       "M5 (LLOQ/2)", "M3 (in the likelihood)"))
