# Individual concentration-time curves (spaghetti plot): first view the size of the IIV
plot(DATA$TIME, DATA$DV, type = "n", las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
for (id in unique(DATA$ID)) {
  d <- DATA[DATA$ID == id, ]
  lines(d$TIME, d$DV, col = "gray40")
  text(d$TIME[which.max(d$DV)], max(d$DV), id, pos = 3, cex = 0.7)
}
