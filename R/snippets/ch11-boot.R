# Bootstrap: 대상자 단위 복원추출. ID 값으로 뽑고 새 ID 를 부여한다.
# 재현성: seed 명시. bound 는 최대한 넓게 두고, 수렴 실패는 세어서 보고한다.
set.seed(20260827)
B <- 100                                           # 실전은 1000 회 이상
bt <- matrix(NA_real_, 3, B, dimnames = list(c("KA", "V", "K"), NULL))
for (b in 1:B) {
  pick <- sample(uid, replace = TRUE)
  Db <- do.call(rbind, lapply(seq_along(pick), function(i) {
    d <- DATA[DATA$ID == pick[i], ]                # ID "값"으로 추출
    d$ID <- i                                      # 중복 대상자에 새 ID
    d
  }))
  stopifnot(length(unique(Db$ID)) == length(uid))  # 대상자 수 보존 assertion
  fit <- try(silent = TRUE, {
    InitStep(Db, THETAinit = c(3.2, 38, 0.11), OMinit = OMinit,
             SGinit = SGinit, LB = rep(0, 3), UB = rep(1e6, 3),
             Pred = PRED, METHOD = "ZERO")
    EstStep()[["Final Estimates"]][1:3]
  })
  if (!inherits(fit, "try-error")) bt[, b] <- fit
}
ok <- !is.na(bt[1, ])
c(n.success = sum(ok), n.fail = sum(!ok))

ci <- apply(bt[, ok], 1, quantile, c(0.025, 0.5, 0.975))
round(rbind(ci, asymptotic.SE = cov.fo[["Standard Error"]][1:3]), 4)
