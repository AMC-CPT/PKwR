# 전용 도구: R 기본 nls 와 저자의 wnl 패키지
n1 <- nls(DV ~ A*exp(-k*x), d4, start = c(A = 50, k = 0.1))
round(summary(n1)$coefficients, 4)            # 등가중(OLS)이 기본이다

library(wnl)                                  # 오차 모형을 지정한다
fx <- function(TH) TH[1]*exp(-TH[2]*d4$x)
r4 <- nlr(fx, d4, pNames = c("A", "k"), IE = c(50, 0.1), Error = "P",
          LB = c(1, 0.01), UB = c(1e3, 5),
          SecNames = c("half.life"), SecForms = c(~log(2)/k))
round(r4$Est, 4)
