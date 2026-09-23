# Oral dosing: same model, only the initial condition changes (Fa x D in the gut lumen)
ypo <- y0; ypo[["AGL"]] <- Fa*D
po  <- as.data.frame(lsoda(ypo, tsim, dydt, p))

auc <- function(t, C) sum(diff(t)*(head(C, -1) + tail(C, -1))/2)
F.obs <- auc(po$time, po$VEN)/auc(iv$time, iv$VEN)
round(c(F.obs = F.obs, F.pred = Fa*(1 - ER)), 4)

plot(iv$time, iv$VEN, type = "l", las = 1, bty = "l", xlim = c(0, 12),
     ylim = c(0.01, 32), log = "y", yaxt = "n", xlab = "Time (hr)",
     ylab = "Concentration (mg/L)")
axis(2, at = 10^(-2:1), labels = c("0.01", "0.1", "1", "10"), las = 1)
lines(po$time, po$VEN, lty = 2)
lines(po$time, po$LI/Kp[["LI"]], lty = 3)
legend("topright", bty = "n", cex = 0.85, lty = 1:3,
       legend = c("IV, venous", "PO, venous", "PO, liver outflow"))
