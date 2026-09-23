# S3 (Predictions): DV vs PRED / IPRED. Plot IDs, not points, and join within subject.
gofplot <- function(x, y, xlab, ylab) {
  lim <- range(c(x, y))
  plot(x, y, type = "n", las = 1, bty = "l", xlim = lim, ylim = lim,
       xlab = xlab, ylab = ylab)
  abline(0, 1, lty = 3)
  for (id in unique(TAB$ID)) {
    i <- TAB$ID == id
    lines(x[i], y[i], col = "gray70")
    text(x[i], y[i], id, cex = 0.85)
  }
}
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
gofplot(TAB$PRED,    TAB$DV, "PRED (mg/L)",  "DV (mg/L)")
title("(a) population", font.main = 1, cex.main = 1)
gofplot(TAB$CIPREDI, TAB$DV, "IPRED (mg/L)", "DV (mg/L)")
title("(b) individual", font.main = 1, cex.main = 1)
