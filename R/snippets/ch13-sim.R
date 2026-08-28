# 2x2 교차 BE 자료의 시뮬레이션. 개체(로그정규 개체간 변이) 위에 시기,
# 제형 효과와 개체내 오차를 로그척도에서 더한다. 열 이름(SUBJ, GRP, PRD,
# TRT)은 BE 패키지의 관례를 따르며, GRP 는 순서군(RT/TR)이다.
set.seed(20260827)                       # 재현성 [필수]. 개체 단위 생성
n    <- 24                               # 순서군당 12명
seqv <- rep(c("RT", "TR"), each = n/2)   # 대상자별 순서군
d13  <- data.frame(SUBJ = rep(1:n, each = 2), GRP = rep(seqv, each = 2),
                   PRD  = rep(1:2, n))
d13$TRT <- ifelse((d13$GRP == "RT") == (d13$PRD == 1), "R", "T")

iiv <- rnorm(n, 0, 0.35)                 # 개체간 SD 0.35 (CVb 약 36%)
sWa <- sqrt(log(1 + 0.20^2))             # AUC 의 개체내 CV 20%
sWc <- sqrt(log(1 + 0.26^2))             # Cmax 의 개체내 CV 26%
gmr <- c(AUClast = 0.95, Cmax = 1.06)    # 참 GMR (T/R)
d13$AUClast <- round(exp(log(2000) + iiv[d13$SUBJ] + 0.03*(d13$PRD == 2) +
                     log(gmr["AUClast"])*(d13$TRT == "T") +
                     rnorm(2*n, 0, sWa)), 1)
d13$Cmax    <- round(exp(log(480) + iiv[d13$SUBJ] + 0.02*(d13$PRD == 2) +
                     log(gmr["Cmax"])*(d13$TRT == "T") +
                     rnorm(2*n, 0, sWc)), 1)
head(d13, 4)

# 모형 없이 본 소박한 요약: 제형별 기하평균과 그 비
lg <- aggregate(cbind(AUClast = log(AUClast), Cmax = log(Cmax)) ~ TRT,
                d13, mean)
gm <- exp(as.matrix(lg[, -1])); rownames(gm) <- lg$TRT
round(gm, 1)
round(gm["T", ]/gm["R", ], 4)
