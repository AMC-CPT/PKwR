library(wnl)                       # author's nonlinear regression package (needs DV)
t <- dat2$Time
fx1 <- function(TH) TH[1]*exp(-TH[2]*t)                        # 1-compartment
fx2 <- function(TH) TH[1]*exp(-TH[2]*t) + TH[3]*exp(-TH[4]*t)  # 2-compartment

r1 <- nlr(fx1, dat2, pNames = c("C0", "k"), IE = c(15, 0.1),
          LB = c(0.1, 1e-3), UB = c(1e3, 10), Error = "P")
r2 <- nlr(fx2, dat2, pNames = c("A", "alpha", "B", "beta"), Error = "P",
          IE = c(A.hat, alpha.hat, B.hat, beta.hat))    # initial values from stripping
round(r2$Est, 4)
c(AIC.1comp = r1$AIC, AIC.2comp = r2$AIC)
