# 함수: 이 책의 모든 모형이 함수 하나로 표현된다
decay <- function(t, A = 100, k = 0.25) A*exp(-k*t)
round(decay(c(0, 2, 4)), 3)                   # 기본값이 있으면 생략할 수 있다
round(decay(2, k = 0.5), 3)                   # 이름으로 넘기면 순서가 자유롭다

# 마지막 식의 값이 반환값이다. 여러 개를 돌려주려면 list 나 벡터로 묶는다.
summ <- function(v) c(n = length(v), mean = mean(v), sd = sd(v))
round(summ(decay(seq(0, 10, 2))), 3)

# 함수를 돌려주는 함수(클로저). 파라미터를 고정한 새 함수를 찍어낸다.
maker <- function(k) function(t) decay(t, k = k)
fast <- maker(1.0); slow <- maker(0.1)
round(c(fast.t2 = fast(2), slow.t2 = slow(2)), 3)
