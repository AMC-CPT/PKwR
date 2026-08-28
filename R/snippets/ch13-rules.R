# 버려진 규칙 하나를 직접 적용해 본다. 75/75 규칙은 개체별 T/R 비의
# 75% 이상이 (0.75, 1.25) 안에 들 것을 요구했다.
rr    <- with(d13, tapply(AUClast, list(SUBJ, TRT), mean))
ratio <- rr[, "T"]/rr[, "R"]
round(c(n = length(ratio), within = mean(ratio > 0.75 & ratio < 1.25),
        pass = mean(ratio > 0.75 & ratio < 1.25) >= 0.75), 3)

# 이 규칙이 무너지는 자리는 분명하다. 두 제형이 '완전히 같아도'(GMR = 1)
# 개체별 비에는 개체내 오차가 두 번 실리므로 로그 SD 가 sqrt(2)*sw 가 된다.
# 그래서 통과가 기대되는 비율을 닫힌 식으로 쓸 수 있다.
frac <- function(cv) { s <- sqrt(2*log(1 + cv^2))
                       pnorm(log(1.25)/s) - pnorm(log(0.75)/s) }
cw <- c(0.10, 0.15, 0.20, 0.25, 0.30)
round(rbind(CVw = cw, expected.within = frac(cw), rule.needs = 0.75), 3)

# 80/125 는 로그척도에서 대칭이고 (+-20%) 규칙은 그렇지 않다
round(c(lo = log(0.80), hi = log(1.25), sum = log(0.80) + log(1.25),
        pm20.hi = log(1.20), pm20.sum = log(0.80) + log(1.20)), 4)
