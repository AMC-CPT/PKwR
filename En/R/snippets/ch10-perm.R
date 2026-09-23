# Permeability-limited: if membrane permeation is slow, the tissue cannot keep up
# with the blood flow. The brain is the example.
Cart <- approxfun(iv$time, iv$ART, rule = 2)      # arterial conc. from whole-body model
perm <- function(PS, org = "BR", fv = 0.03) {     # fv: vascular fraction of the tissue
  Vt <- V[[org]]; Vv <- Vt*fv; Vi <- Vt - Vv
  d <- function(t, y, .) { J <- PS*(y[1] - y[2]/Kp[[org]])   # membrane permeation flux
    list(c((Q[[org]]*(Cart(t) - y[1]) - J)/Vv, J/Vi)) }
  o <- as.data.frame(lsoda(c(Cv = 0, Ct = 0), tsim, d, NULL))
  o$Ct }                                          # extravascular tissue concentration

PSv <- c(0.5, 5, 50, 500)                         # L/hr (brain blood flow is 42 L/hr)
Cp  <- sapply(PSv, perm)                          # for the 4 values of PS
summ <- function(x) c(Cmax = max(x), tmax = tsim[which.max(x)],
                      AUC = auc(tsim, x))
tab <- cbind(sapply(seq_along(PSv), function(j) summ(Cp[, j])),
            perfusion = summ(iv$BR))
colnames(tab)[seq_along(PSv)] <- paste0("PS=", PSv)
round(rbind(tab, PS.over.Q = c(PSv/Q[["BR"]], Inf)), 3)

matplot(tsim, Cp, type = "l", lty = 2:5, col = 1, las = 1, bty = "l",
        xlim = c(0, 8), ylim = c(0, 4), xlab = "Time (hr)",
        ylab = "Brain concentration (mg/L)")
lines(iv$time, iv$BR, lwd = 1.8)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2:5), lwd = c(1.8, rep(1, 4)),
       legend = c("perfusion-limited", paste("PS =", PSv)))
