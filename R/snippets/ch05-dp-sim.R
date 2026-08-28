# 용량 비례성 시험 자료를 만든다: 12명 x 3용량 교차설계, 경구 1구획 모형.
# 개인간 변이는 CL 과 V 에 로그정규(CV 25%, 20%)로 준다.
nsub  <- 12
doses <- c(100, 300, 900)
tobs  <- c(0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24)

set.seed(20260821)
CLi <- 5.0 * exp(rnorm(nsub, 0, 0.25))     # L/hr
Vi  <-  50 * exp(rnorm(nsub, 0, 0.20))     # L
kai <- 1.2 * exp(rnorm(nsub, 0, 0.30))     # /hr

dat <- do.call(rbind, lapply(seq_len(nsub), function(i)
  do.call(rbind, lapply(doses, function(D) data.frame(
    Subject = i, Dose = D, Time = tobs,
    conc = Cpo(tobs, D = D*1000, V = Vi[i], k = CLi[i]/Vi[i], ka = kai[i]) *
             exp(rnorm(length(tobs), 0, 0.08)))))))   # 측정오차 8%

str(dat)
head(dat, 4)
