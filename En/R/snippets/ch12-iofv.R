# Individual OFV: split the total OFV into subject contributions (formula in the text)
iOFV <- sapply(unique(DATA$ID), function(id) {
  Di <- DATA[DATA$ID == id, ]
  Ei <- as.numeric(EBE[EBE[, "ID"] == id, 2:4])
  P  <- PRED(TH, Ei, Di)
  Ri <- Di$DV - P[, "F"]
  Vi <- diag(P[, c("H1", "H2")] %*% SG %*% t(P[, c("H1", "H2")]))
  Gi <- P[, c("G1", "G2", "G3")]
  qi <- sum(log(Vi) + Ri^2/Vi) + Ei %*% solve(OM) %*% Ei
  as.numeric(qi + log(det(OM)) +
             log(det(solve(OM) + t(Gi) %*% diag(1/Vi) %*% Gi)))
})
c(sum.iOFV = sum(iOFV), OFV = r.foce$Optim$value)

nDV <- table(DATA$ID)                              # observations per subject
worst <- order(iOFV/nDV, decreasing = TRUE)        # order of worst fit
round(data.frame(ID = unique(DATA$ID), iOFV = iOFV, nDV = as.numeric(nDV),
                 OFVpDV = iOFV/as.numeric(nDV))[worst[1:5], ], 2)
