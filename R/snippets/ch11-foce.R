# FOCE-I: METHOD 만 바꾸면 된다. 매 반복마다 개인별 EBE 탐색이 들어가
# FO 보다 훨씬 느리다 (이 자료로 1분 남짓).
InitStep(DATA, THETAinit = THETAinit, OMinit = OMinit, SGinit = SGinit,
         LB = rep(0, 3), UB = rep(1e6, 3), Pred = PRED, METHOD = "COND")
r.foce <- EstStep()
FE <- r.foce[["Final Estimates"]]
round(FE, 4)
c(OFV.FO = r.fo$Optim$value, OFV.FOCEI = r.foce$Optim$value)

# 하삼각(행 우선) 벡터 -> 대칭 OMEGA 행렬, 그리고 해석 가능한 요약
utri <- matrix(0, 3, 3)
utri[upper.tri(utri, diag = TRUE)] <- FE[4:9]
OM <- utri + t(utri) - diag(diag(utri))
SG <- diag(FE[10:11]); TH <- FE[1:3]
round(100*sqrt(exp(diag(OM)) - 1), 1)         # IIV CV% (로그정규)
round(cov2cor(OM), 3)                         # eta 간 상관
