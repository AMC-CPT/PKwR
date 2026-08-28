# 2단계 순차 설계 (Potvin Method C): 1단계 12명으로 시작한다.
# 계획은 CV 20% 였지만 실제 개체내 CV 가 그보다 크다면?
set.seed(20260827)
n1  <- 12
st1 <- data.frame(SUBJ = rep(1:n1, each = 2), Stage = 1,
                  GRP  = rep(rep(c("RT", "TR"), each = n1/2), each = 2),
                  PRD  = rep(1:2, n1))
st1$TRT <- ifelse((st1$GRP == "RT") == (st1$PRD == 1), "R", "T")
sWt <- sqrt(log(1 + 0.26^2))              # 실제 개체내 CV 26% (참 GMR 은 1.0)
st1$AUClast <- round(exp(log(1500) + rnorm(n1, 0, 0.3)[st1$SUBJ] +
                     0.02*(st1$PRD == 2) + log(1.00)*(st1$TRT == "T") +
                     rnorm(2*n1, 0, sWt)), 1)
st1f <- af(st1, c("SUBJ", "GRP", "PRD", "TRT"))

# 1단계 검정력(alpha = 0.05 기준)이 80% 미만이므로 조정 alpha 로 판정한다
a2 <- 0.0294                              # Pocock K = 2 의 명목 유의수준
f1 <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
mse1 <- GLM(f1, st1f)$ANOVA["RESIDUALS", "Mean Sq"]
CV1  <- mse2cv(mse1)/100
library(PowerTOST)
pw1 <- power.TOST(CV = CV1, theta0 = 0.95, n = n1, method = "central")
ci1 <- setNames(exp(CIest(f1, st1f, "TRT", c(-1, 1),
                    conf.level = 1 - 2*a2)[1, 1:3]), c("PE", "LL", "UL"))
round(c(CV1 = CV1, power.at.0.05 = pw1), 4)
round(ci1, 4)                             # 94.12% CI: 80-125 를 벗어난다

# 관측 CV 로 표본을 재산정한다 (조정 alpha, 참 GMR 은 계획값 0.95 유지)
n.new <- 2*ssmse(mse1, True.R = 0.95, Alpha = 2*a2)
c(n.new = n.new, n2 = n.new - n1)
