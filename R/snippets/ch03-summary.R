# 요약통계는 무엇을 추정하는가: 로그정규 표본 하나를 세 가지로 요약한다
set.seed(20260828)
x <- rlnorm(200, mu, sg)
gm  <- function(v) exp(mean(log(v)))
gcv <- function(v) sqrt(exp(var(log(v))) - 1)*100
round(c(arith.mean = mean(x), geo.mean = gm(x), median = median(x),
        SD = sd(x), gCV.pct = gcv(x)), 3)

# 참값과 견주면 무엇이 무엇의 추정치인지 분명해진다
round(c(true.arith = exp(mu + sg^2/2), true.geo = exp(mu),
        true.median = exp(mu), true.CV.pct = 100*sqrt(exp(sg^2) - 1)), 3)
