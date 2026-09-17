# 벡터 하나에는 한 가지 형만 담긴다. 섞으면 넓은 쪽으로 조용히 바뀐다.
c(num = class(c(1, 2)), mixed = class(c(1, "2")), lgl = class(c(TRUE, 1)))
as.numeric(c("1.5", "abc"))                   # 바꿀 수 없으면 NA (경고와 함께)

# 뜻이 다른 특수값 넷. 로그 축과 BLQ 를 다룰 때 반드시 만난다.
z <- c(0/0, 1/0, NA, 3)
rbind(is.na = is.na(z), is.nan = is.nan(z), is.finite = is.finite(z))
c(log.of.0 = log(0), zero.times.Inf = 0*Inf)

# 요인(factor): 숫자로 코딩된 범주를 '수준'으로 바꾼다. 14장 분산분석이 이것을 쓴다.
grp <- c(1, 2, 3, 1, 2, 3); yy <- c(5, 7, 12, 6, 8, 11)
c(as.number = df.residual(lm(yy ~ grp)),      # 기울기 하나짜리 회귀가 된다
  as.factor = df.residual(lm(yy ~ factor(grp))))
