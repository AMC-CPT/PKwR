# 투여력(dosing history): 0 h 정맥 일시주입 100 mg, 24 h 부터 50 mg/hr 로 150 mg
# 주입, 48 h 경구 100 mg. CMT 1 = 흡수부위(gut), CMT 2 = 중심구획.
library(wnl)
dh <- data.frame(
  TIME = c(0, 1, 2, 4, 8, 12, 24, 25, 26, 28, 32, 36, 48, 49, 50, 52, 56, 60),
  AMT  = c(100, rep(NA, 5), 150, rep(NA, 5), 100, rep(NA, 5)),
  RATE = c(  0, rep(NA, 5),  50, rep(NA, 5),   0, rep(NA, 5)),
  CMT  = c(  2, rep(NA, 5),   2, rep(NA, 5),   1, rep(NA, 5)))

# 해석해는 '입력이 바뀌지 않는 구간' 안에서만 한 줄로 성립한다. 주입이 끝나는
# 27 h 는 관측 시각이 아니지만 미분이 끊기므로 반드시 격자에 넣어야 한다.
dh2 <- ExpandDH(dh)
dh2[, c("TIME", "AMT", "RATE", "CMT", "BOLUS", "RATE2")]
