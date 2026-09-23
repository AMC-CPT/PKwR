# Four indirect response types: the drug inhibits (I) or stimulates (S) the production
# (kin) or elimination (kout) of the turnover system dR/dt = kin - kout R.
# Solved with deSolve, which was used in Chapter 10.
library(deSolve)
kin <- 9; kout <- 0.15; R0 <- kin/kout            # baseline R0 = 60
k <- 0.3; V <- 10                                 # drug: 1-compartment IV, t1/2 2.3 hr
Imax <- 0.8; IC50 <- 3; Smax <- 2; SC50 <- 3

didr4 <- function(type, D) function(t, y, p) {
  Cp <- D/V*exp(-k*t)
  I  <- Imax*Cp/(IC50 + Cp) ; S <- Smax*Cp/(SC50 + Cp)
  list(switch(type, I   = kin*(1 - I) - kout*y,
                    II  = kin - kout*(1 - I)*y,
                    III = kin*(1 + S) - kout*y,
                    IV  = kin - kout*(1 + S)*y))
}
t  <- seq(0, 72, 0.1); D <- c(50, 150, 450); ty <- c("I", "II", "III", "IV")
R  <- lapply(ty, function(m) sapply(D, function(d)
        lsoda(c(R = R0), t, didr4(m, d), NULL)[, "R"]))

par(mfrow = c(2, 2), mar = c(4.0, 4.2, 2.2, 0.8))
lab <- c("(I) inhibit kin", "(II) inhibit kout",
         "(III) stimulate kin", "(IV) stimulate kout")
for (i in 1:4) {
  matplot(t, R[[i]], type = "l", lty = 1:3, col = 1, las = 1, bty = "l",
          xlab = "Time (hr)", ylab = "Response", main = lab[i], cex.main = 0.95)
  abline(h = R0, lty = 3)
  if (i == 1) legend("topright", bty = "n", cex = 0.8, lty = 1:3,
                     legend = paste("D =", D))
}

# Time of the maximum (or minimum) response: later at higher doses in all four types
TRm <- sapply(R, function(m) t[apply(abs(m - R0), 2, which.max)])
dimnames(TRm) <- list(paste("D =", D), ty); TRm

# Limiting responses attainable as the dose is raised without bound
round(c(R0 = R0, lim.I = R0*(1 - Imax), lim.II = R0/(1 - Imax),
        lim.III = R0*(1 + Smax), lim.IV = R0/(1 + Smax)), 1)
