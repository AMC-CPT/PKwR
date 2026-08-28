# 표준 2x2 분석: log(Y) ~ 순서군 + 순서군 내 개체 + 시기 + 제형.
# sasLM 은 SAS PROC GLM 과 같은 결과를 준다 (저자 패키지).
library(sasLM)
d13f <- af(d13, c("SUBJ", "GRP", "PRD", "TRT"))       # 요인화
f1 <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
GLM(f1, d13f)$`Type III`

# 순서군(GRP)만은 잔차가 아니라 '개체' 항을 오차로 검정해야 한다
RanTest(f1, d13f, Random = "SUBJ")$GRP

# GMR 의 90% 신뢰구간: T - R 로그차를 추정해 지수변환한다
ci <- setNames(exp(CIest(f1, d13f, "TRT", c(-1, 1),
                         conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL"))
mse <- GLM(f1, d13f)$ANOVA["RESIDUALS", "Mean Sq"]
round(c(ci, CVw.pct = BE::mse2cv(mse)), 4)
c(BE = unname(ci["LL"] >= 0.8 & ci["UL"] <= 1.25))

# Cmax 도 같은 모형으로
round(setNames(exp(CIest(log(Cmax) ~ GRP/SUBJ + PRD + TRT, d13f, "TRT",
      c(-1, 1), conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL")), 4)
