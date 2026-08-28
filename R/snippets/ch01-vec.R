# R 의 기본 단위는 스칼라가 아니라 벡터다. 연산이 원소마다 한꺼번에 일어난다.
x <- c(2.4, 5.1, 7.8, 3.3, 9.0)
c(length = length(x), sum = sum(x), mean = mean(x))
round(x/max(x), 3)                            # 벡터 나누기 스칼라: 재활용

# 규칙적인 벡터를 만드는 두 방법
seq(0, 12, by = 3)
rep(c("A", "B"), times = 3)

# 인덱싱: 위치, 논리, 이름 세 가지
x[2]; x[c(1, 5)]; x[-1]                       # 음수는 '빼고'
x[x > 5]                                      # 논리 첨자가 가장 많이 쓰인다
names(x) <- c("a", "b", "c", "d", "e"); x[["c"]]

# 결측은 전염된다. 함수마다 처리 방법을 명시해야 한다.
y <- c(1, NA, 3)
c(sum.default = sum(y), sum.narm = sum(y, na.rm = TRUE), n.missing = sum(is.na(y)))
