EBE <- get("EBE", envir = e)            # FOCE 는 EBE 를 이미 갖고 있다
round(head(EBE, 3), 3)

# eta-shrinkage: EBE 의 표준편차가 추정된 omega 보다 얼마나 수축했는가
shr <- 1 - apply(EBE[, 2:4], 2, sd)/sqrt(diag(OM))
round(100*shr, 1)
