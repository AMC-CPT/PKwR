# 표본크기: 개체내 CV 와 예상 GMR 이 입력의 전부다. 저자의 BE 패키지
# (sscv, CV 를 % 로 받고 '순서군당' 인원을 준다)와 PowerTOST
# (sampleN.TOST, CV 를 분율로 받고 총 인원을 준다) 어느 쪽으로도 구한다.
library(PowerTOST)
2*sscv(CV = 25, True.R = 0.95)                     # BE 패키지: 총 28명
sampleN.TOST(CV = 0.25, theta0 = 0.95, design = "2x2", targetpower = 0.8,
             print = FALSE)[c("Sample size", "Achieved power")]

# CV x GMR 격자: 검정력 80% 에 필요한 총 대상자 수 (정확법)
cvs <- c(0.15, 0.20, 0.25, 0.30, 0.35)
tab <- sapply(c(0.95, 0.90), function(r) sapply(cvs, function(cv)
         sampleN.TOST(CV = cv, theta0 = r, design = "2x2",
                      print = FALSE)[["Sample size"]]))
dimnames(tab) <- list(paste0("CV ", 100*cvs, "%"), paste("GMR", c(0.95, 0.90)))
tab

# BE 는 AUC 와 Cmax 가 '모두' 통과해야 하므로 진짜 검정력은 두 지표의
# 결합 검정력이고, 둘의 상관을 고려해야 정확하다 (power.2TOST).
# n = 28, AUC CV 20%/GMR 0.95, Cmax CV 26%/GMR 1.06, 상관 0.75 라면:
round(c(AUC.alone  = power.TOST(CV = 0.20, theta0 = 0.95, n = 28),
        Cmax.alone = power.TOST(CV = 0.26, theta0 = 1.06, n = 28),
        joint = power.2TOST(theta0 = c(0.95, 1.06), CV = c(0.20, 0.26),
                            n = 28, rho = 0.75)), 4)
