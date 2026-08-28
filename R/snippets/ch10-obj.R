# FOCE-I 의 목적함수를 손으로 열어 본다. 필요한 것은 EBE 와 최종 추정치뿐.
iOM <- solve(OM); lgOM <- determinant(OM, logarithm = TRUE)$modulus[[1]]
OFVi <- sapply(seq_len(nrow(EBE)), function(i) {
  Di <- DATA[DATA$ID == EBE[i, "ID"], ]
  et <- EBE[i, c("ETA1", "ETA2", "ETA3")]
  M  <- PRED(TH, et, as.matrix(Di))          # F, G(=dF/deta), H(=dY/deps)
  Vi <- diag(M[, c("H1", "H2")] %*% SG %*% t(M[, c("H1", "H2")]))
  Gi <- M[, c("G1", "G2", "G3")]
  Ri <- Di$DV - M[, "F"]
  Phi <- sum(log(Vi) + Ri^2/Vi)              # 개인 자료의 -2LL (상수 제외)
  Phi + drop(t(et) %*% iOM %*% et) + lgOM +  # 사전(벌점) 항 + log|OMEGA|
    determinant(iOM + t(Gi) %*% diag(1/Vi) %*% Gi, logarithm = TRUE)$modulus[[1]]
})
round(c(OFV.byhand = sum(OFVi), OFV.nmw = r.foce$Optim$value,
        difference = sum(OFVi) - r.foce$Optim$value), 4)
round(head(OFVi, 4), 3)                      # 개인별 목적함수 기여
