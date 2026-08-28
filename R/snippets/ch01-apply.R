# 반복은 for 루프로도 되지만, apply 계열이 짧고 결과 형태가 분명하다
ks <- c(0.1, 0.25, 0.5)
out <- numeric(length(ks))                    # for: 담을 그릇을 미리 만든다
for (i in seq_along(ks)) out[i] <- decay(4, k = ks[i])
round(out, 3)

round(sapply(ks, function(k) decay(4, k = k)), 3)      # sapply: 벡터로 돌려준다
str(lapply(ks, function(k) decay(c(0, 4), k = k)))     # lapply: 리스트로

# 행렬의 행/열 요약은 apply, 리스트를 한 표로 붙이는 것은 do.call(rbind, .)
m <- sapply(ks, function(k) decay(c(0, 2, 4), k = k))
round(apply(m, 2, sum), 3)                    # 2 = 열 방향
do.call(rbind, lapply(ks, function(k) c(k = k, y4 = round(decay(4, k = k), 3))))

# 같은 계산을 난수와 함께 여러 번 반복할 때는 replicate
set.seed(20260828)
round(quantile(replicate(1000, mean(rnorm(5))), c(0.025, 0.975)), 3)
