# 개인별 OFV: 전체 OFV 를 대상자별 기여로 분해한다 (식은 본문 참조)
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

nDV <- table(DATA$ID)                              # 대상자별 관측 수
worst <- order(iOFV/nDV, decreasing = TRUE)        # 적합이 나쁜 순서
round(data.frame(ID = unique(DATA$ID), iOFV = iOFV, nDV = as.numeric(nDV),
                 OFVpDV = iOFV/as.numeric(nDV))[worst[1:5], ], 2)
