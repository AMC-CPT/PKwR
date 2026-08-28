# 초기값은 자료에서 읽는다: 반로그 직선의 절편과 기울기
lf <- lm(log(DV) ~ x, d4)
ie <- c(A = exp(coef(lf)[[1]]), k = -coef(lf)[[2]])
round(c(ie, SSE = sse(ie)), 4)

# 이 문제는 초기값에 둔감하다: 9개 출발점이 모두 같은 답으로 모인다
starts <- expand.grid(A = c(5, 50, 500), k = c(0.02, 0.2, 2))
sol <- t(apply(starts, 1, function(s)
  optim(as.numeric(s), sse, method = "L-BFGS-B",
        lower = c(1e-3, 1e-3), upper = c(1e4, 20))$par))
c(n.start = nrow(sol), n.distinct = nrow(unique(round(sol, 2))))
round(range(sol[, 2]), 4)                     # k 추정치의 범위
