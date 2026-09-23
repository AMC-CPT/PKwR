# Two-compartment IV bolus:  C(t) = A exp(-a t) + B exp(-b t)
C2 <- function(t, A = 85, a = 1.0, B = 15, b = 0.13)
  A*exp(-a*t) + B*exp(-b*t)

t <- seq(0, 24, 0.05)
plot(t, C2(t), type = "l", log = "y", las = 1, bty = "l",
     ylim = c(0.4, 130), xlab = "Time", ylab = "Concentration")
text( 2.4, 26,  "Distribution phase", pos = 4, cex = 0.85)
text( 7.0, 0.85, "Elimination phase", pos = 4, cex = 0.85)

# Terminal half-life from the slope of the last linear segment (t >= 12).
tail.t <- t[t >= 12]
fit <- lm(log(C2(tail.t)) ~ tail.t)
lambda.z <- unname(-coef(fit)[2])
c(lambda.z = lambda.z, t.half.z = log(2)/lambda.z)
