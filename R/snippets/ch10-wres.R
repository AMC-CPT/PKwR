# 같은 적합에서 두 가지 가중잔차를 만든다. 차이는 '어디서 전개했는가'뿐이다.
wres2 <- function(inter) do.call(rbind, lapply(seq_len(nrow(EBE)), function(i) {
  Di <- DATA[DATA$ID == EBE[i, "ID"], ]
  et <- EBE[i, c("ETA1", "ETA2", "ETA3")]
  M0 <- PRED(TH, c(0, 0, 0), as.matrix(Di))          # eta = 0 에서 전개 (FO)
  M1 <- PRED(TH, et,         as.matrix(Di))          # EBE 에서 전개 (FOCE)
  M  <- if (inter) M1 else M0
  G  <- M[, c("G1", "G2", "G3")]; H <- M[, c("H1", "H2")]
  Ci <- G %*% OM %*% t(G) + diag(diag(H %*% SG %*% t(H)))
  Ri <- if (inter) Di$DV - M1[, "F"] + G %*% et else Di$DV - M0[, "F"]
  cbind(TIME = Di$TIME, R = drop(SqrtInvCov(Ci) %*% Ri))
}))
w0 <- wres2(FALSE); w1 <- wres2(TRUE)                # WRES 와 CWRES
round(c(SD.WRES = sd(w0[, "R"]), SD.CWRES = sd(w1[, "R"]),
        max.abs.WRES = max(abs(w0[, "R"])), max.abs.CWRES = max(abs(w1[, "R"]))), 3)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (w in list(list(w0, "(a) WRES  (eta = 0 전개)"),
               list(w1, "(b) CWRES (EBE 전개)"))) {
  plot(w[[1]][, "TIME"], w[[1]][, "R"], las = 1, bty = "l", pch = 1, cex = 0.7,
       ylim = c(-4.5, 4.5), xlab = "Time (hr)", ylab = "Weighted residual",
       main = w[[2]], cex.main = 0.95)
  abline(h = 0, lty = 3); lines(lowess(w[[1]][, "TIME"], w[[1]][, "R"]), lty = 2)
}
