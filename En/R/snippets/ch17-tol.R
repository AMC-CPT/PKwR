# Tolerance (antagonistic mediator model): the drug concentration drives formation of
# a hypothetical mediator Cm, and the effect erodes as Cm grows. dCm/dt = km0 (Cp - Cm)
# has the same form as the effect compartment, so the analytic solutions for each
# exponential term of the first-order absorption input are superposed (linear system).
D <- 150; V <- 15; ka <- 1.2; k <- 0.35; tau <- 12  # two equal doses, 12 hours apart
km0 <- 0.06; Cm50 <- 2; E0 <- 60; Sl <- 8           # mediator elimination t1/2 11.6 hr
C1  <- D/V*ka/(ka - k)
cp1 <- function(t) ifelse(t < 0, 0, C1*(exp(-k*pmax(t, 0)) - exp(-ka*pmax(t, 0))))
g   <- function(t, a) ifelse(t < 0, 0,
         km0/(km0 - a)*(exp(-a*pmax(t, 0)) - exp(-km0*pmax(t, 0))))
cm1 <- function(t) C1*(g(t, k) - g(t, ka))

t  <- seq(0, 36, 0.02)
Cp <- cp1(t) + cp1(t - tau)
Cm <- cm1(t) + cm1(t - tau)
E     <- E0 + Sl*Cp/(1 + Cm/Cm50)                   # actual effect including tolerance
E.ref <- E0 + Sl*Cp                                 # effect if there were no mediator

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(t, E.ref, type = "l", lty = 3, las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Effect", main = "(a) time course")
lines(t, E)
legend("topright", bty = "n", cex = 0.8, lty = c(3, 1),
       legend = c("no tolerance", "with mediator"))

i1 <- t <= tau                                      # loop over the first dose interval
plot(Cp[i1], E[i1], type = "l", las = 1, bty = "l",
     xlab = "Concentration", ylab = "Effect", main = "(b) proteresis (dose 1)")
j <- c(30, 500)                                     # rising (t=0.6) and falling (t=10)
arrows(Cp[j], E[j], Cp[j + 15], E[j + 15], length = 0.07)

# Accumulation makes the second Cmax higher, yet the second peak of effect is lower
i2 <- t >= tau
round(c(Cp.max1 = max(Cp[i1]), Cp.max2 = max(Cp[i2]),
        E.max1 = max(E[i1]), E.max2 = max(E[i2])), 1)
