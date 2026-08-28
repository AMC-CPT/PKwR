# RPT: 공변량(BWT)을 대상자 사이에서 무작위로 섞어 "효과가 없는 세계"의
# dOFV 분포를 만든다. 시간이 걸리므로 빠른 FO 로 한다(실전은 최종 추정법).
# 재현성: seed 명시, 재추출 단위 = 대상자, 비복원 섞기(permutation).
fo.ofv <- function(D, PredF, IE, LB, UB) {
  InitStep(D, THETAinit = IE, OMinit = OMinit, SGinit = SGinit,
           LB = LB, UB = UB, Pred = PredF, METHOD = "ZERO")
  EstStep()$Optim$value
}
IE4 <- c(3.2, 38, 0.11, 0.5)                       # FO 기반 초기값
ofv.full <- fo.ofv(DATA, PRED4, IE4, c(0, 0, 0, -5), c(1e6, 1e6, 1e6, 5))
dOFV.obs <- r.fo$Optim$value - ofv.full            # 관측 dOFV (FO 기준)

uid <- unique(DATA$ID)
wt0 <- DATA$BWT[match(uid, DATA$ID)]
set.seed(20260827)
null <- replicate(50, {                            # 실전은 1000 회 이상
  Dp <- DATA
  Dp$BWT <- sample(wt0)[match(Dp$ID, uid)]         # match: 행 순서 보존
  stopifnot(all(tapply(Dp$BWT, Dp$ID,              # permutation 후 assertion:
                       function(x) length(unique(x))) == 1),
            all(sort(as.numeric(tapply(Dp$BWT, Dp$ID, unique))) == sort(wt0)))
  r.fo$Optim$value -
    fo.ofv(Dp, PRED4, IE4, c(0, 0, 0, -5), c(1e6, 1e6, 1e6, 5))
})
c(dOFV.obs = round(dOFV.obs, 2),
  p.perm = round((1 + sum(null >= dOFV.obs))/(1 + length(null)), 3),
  dOFV.crit = round(quantile(null, 0.95)[[1]], 2))

hist(null, breaks = 12, las = 1, col = "gray92", main = "",
     xlab = expression(Delta*"OFV under permutation"),
     xlim = range(c(null, dOFV.obs)))
abline(v = dOFV.obs, lwd = 1.4)
text(dOFV.obs, par("usr")[4]*0.9, "observed", pos = 2, cex = 0.85)
