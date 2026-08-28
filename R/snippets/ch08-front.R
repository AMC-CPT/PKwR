# NonCompart 의 C0 규칙(정맥 일시주사에서만): 처음 두 양수 농도가
# 하강(C1 > C2)이면 로그 역외삽, 아니면 첫 농도를 그대로 쓴다
c0.rule <- function(t, C) {
  i <- which(C > 0)[1:2]
  if (C[i[1]] > C[i[2]])
    exp(log(C[i[1]]) - t[i[1]]*diff(log(C[i]))/diff(t[i]))
  else C[i[1]]
}
round(c(C0 = c0.rule(dat2$Time, dat2$DV), AUC.backext = a0,
        pct.backext = a0/AUC.inf*100), 4)
