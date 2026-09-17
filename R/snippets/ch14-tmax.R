# Tmax 는 예정된 채혈 시각에서만 관측되므로 연속형이 아니다. 참 T-R
# 차이를 0.35 시간으로 두고 자료를 만들어, 그 이산성이 어떻게 드러나는지 본다.
set.seed(20260829)                      # 재현성 [필수]. 개체 단위 생성
sch  <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 12, 24)   # 채혈 계획
snap <- function(x) sch[sapply(x, function(v) which.min(abs(sch - v)))]
shft <- 0.35                            # 참: 시험약의 Tmax 가 늦다
dt   <- data.frame(SUBJ = rep(1:n, each = 2), SEQ = rep(seqv, each = 2),
                   PRD = rep(1:2, n))
dt$TRT  <- ifelse((dt$SEQ == "RT") == (dt$PRD == 1), "R", "T")
tsub    <- rnorm(n, 1.2, 0.45)          # 개체별 흡수 속도의 차이
dt$Tmax <- snap(pmax(0.2, tsub[dt$SUBJ] + rnorm(2*n, 0, 0.25)) +
                ifelse(dt$TRT == "T", shft, 0))
table(dt$Tmax, dt$TRT)

# 48개 관측이 7개 값에만 몰려 있다. 동점이 이렇게 많으면 정규성도 등분산도
# 따질 것이 없다. 교차설계의 비모수 분석은 개체별 '시기차'를 순서군끼리
# 비교하는 것이다 (Hauschke 등의 방법). 시기효과는 두 순서군에 같이 실려
# 소거되고, 남는 위치 차이가 제형 효과의 두 배가 된다.
tw  <- with(dt, tapply(Tmax, list(SUBJ, PRD), mean))
dif <- tw[, 1] - tw[, 2]
w   <- wilcox.test(dif ~ seqv, conf.int = TRUE, conf.level = 0.90,
                   exact = FALSE)
round(c(HL.est = -w$estimate[[1]]/2, lower = -w$conf.int[2]/2,
        upper = -w$conf.int[1]/2, p.value = w$p.value, true = shft), 4)

# 같은 자료를 모수적으로 다루면 어떻게 되는지 (참고용)
round(c(t.est = -diff(rev(t.test(dif ~ seqv)$estimate))[[1]]/2,
        median.R = median(dt$Tmax[dt$TRT == "R"]),
        median.T = median(dt$Tmax[dt$TRT == "T"])), 4)
