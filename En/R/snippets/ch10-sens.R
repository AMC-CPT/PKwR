# Sensitivity analysis: % change in result when a parameter rises 10% (normalized)
tsen <- c(seq(0, 24, 0.02), seq(24.5, 120, 0.5))   # integrate out to the tail for AUC
metrics <- function(pp) {
  a <- as.data.frame(lsoda(yiv, tsen, dydt, pp))
  b <- as.data.frame(lsoda(ypo, tsen, dydt, pp))
  c(AUC.iv = auc(a$time, a$VEN), AUC.po = auc(b$time, b$VEN),
    Cmax.po = max(b$VEN)) }
bump <- function(nm, f = 1.1) {
  pp <- p
  if (grepl("^Kp[.]", nm)) { k <- sub("^Kp[.]", "", nm); pp$Kp[k] <- pp$Kp[k]*f }
  else if (grepl("^Q[.]", nm)) { k <- sub("^Q[.]", "", nm)
    pp$Q[k] <- pp$Q[k]*f; pp$CO <- sum(pp$Q[tis]) }        # rebalance the blood flows
  else if (nm == "CO") { pp$Q <- pp$Q*f; pp$CO <- pp$CO*f }
  else pp[[nm]] <- pp[[nm]]*f
  pp }

base <- metrics(p)
pars <- c("CLint", "fu", "GFR", "ka", "Kp.AD", "Kp.MU", "Q.LI", "CO")
S <- t(sapply(pars, function(nm) (metrics(bump(nm))/base - 1)/0.1))
S <- S[order(-abs(S[, "AUC.po"])), ]
round(S, 3)

par(mar = c(4.2, 6.0, 1.0, 1.0))                  # tornado plot
barplot(rev(S[, "AUC.po"]), horiz = TRUE, las = 1, cex.names = 0.85,
        xlab = "Normalized sensitivity (oral AUC)", col = "gray85", border = "gray40")
abline(v = 0, lwd = 1.1)
