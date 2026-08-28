# 조건: 값 하나면 if, 벡터 전체를 한꺼번에 고르면 ifelse
tlag <- 1; tt <- c(0, 0.5, 1, 2, 4)
round(ifelse(tt < tlag, 0, decay(tt - tlag)), 3)   # 지연시간이 있는 흡수

# 조건이 만족될 때까지 반복하는 계산에는 while 과 break
half <- function(k, tol = 1e-10) {            # log(2)/k 를 이분법으로 찾는다
  lo <- 0; hi <- 100
  repeat {
    mid <- (lo + hi)/2
    if (hi - lo < tol) break
    if (exp(-k*mid) > 0.5) lo <- mid else hi <- mid
  }
  mid
}
c(bisection = half(0.25), exact = log(2)/0.25)

# 가정은 주석이 아니라 코드로 적는다. 틀리면 그 자리에서 멈춘다.
chk <- function(d) { stopifnot(!is.unsorted(d$ID), all(d$TIME >= 0)); "OK" }
chk(data.frame(ID = c(1, 1, 2), TIME = c(0, 2, 0)))
cat(try(chk(data.frame(ID = c(2, 1), TIME = c(0, 0))), silent = TRUE))
