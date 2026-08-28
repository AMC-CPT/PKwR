# data.frame: 열마다 형이 다를 수 있는 표. 이 책의 자료는 모두 이 꼴이다.
d1 <- data.frame(ID = rep(1:3, each = 4), TIME = rep(c(0, 2, 4, 8), 3),
                 DV = round(decay(rep(c(0, 2, 4, 8), 3),
                                  A = rep(c(90, 100, 120), each = 4)), 2))
head(d1, 5); dim(d1)

# 열은 $ 또는 [[ ]], 행은 논리 첨자로 고른다
d1$DV[1:3]
d1[d1$ID == 2 & d1$TIME > 0, ]

# 요약: 집단별 계산은 tapply 나 aggregate 가 편하다
round(tapply(d1$DV, d1$ID, max), 2)
aggregate(DV ~ ID, d1, function(v) round(c(max = max(v), min = min(v)), 2))
