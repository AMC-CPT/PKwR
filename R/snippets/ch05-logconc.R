# 2구획 정맥 일시주사:  C(t) = A exp(-a t) + B exp(-b t)
C2 <- function(t, A = 85, a = 1.0, B = 15, b = 0.13)
  A*exp(-a*t) + B*exp(-b*t)

t <- seq(0, 24, 0.05)
plot(t, C2(t), type = "l", log = "y", las = 1, bty = "l",
     ylim = c(0.4, 130), xlab = "Time", ylab = "Concentration")
text( 2.4, 26,  "Distribution phase", pos = 4, cex = 0.85)
text( 7.0, 0.85, "Elimination phase", pos = 4, cex = 0.85)

# 마지막 직선 구간(t >= 12)의 기울기로 최종반감기를 구한다.
tail.t <- t[t >= 12]
fit <- lm(log(C2(tail.t)) ~ tail.t)
lambda.z <- unname(-coef(fit)[2])
c(lambda.z = lambda.z, t.half.z = log(2)/lambda.z)
