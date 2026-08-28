# 행렬: 같은 형의 값을 행과 열로 놓은 것. 10장의 분산-공분산이 모두 이 꼴이다.
M <- matrix(c(0.09, 0.03, 0.03, 0.04), nrow = 2,
            dimnames = list(c("CL", "V"), c("CL", "V")))
M
c(nrow = nrow(M), det = det(M))
round(solve(M), 3)                            # 역행렬 (10·14장에서 쓴다)
round(cov2cor(M), 3)                          # 공분산 -> 상관
round(M %*% c(1, 1), 3)                       # %*% 는 행렬곱, * 는 원소별 곱

# 리스트: 형도 길이도 다른 것을 이름으로 묶는다. 적합 함수의 반환값이 이 꼴이다.
fit <- list(par = c(A = 100, k = 0.25), n = 7L, cov = M)
c(one.bracket = class(fit["par"]), two.brackets = class(fit[["par"]]))
fit$par[["k"]]
names(fit)
