# New hypothetical patient: evaluating a method needs known truth (Chapter 7 strategy).
# Create a patient whose CL far exceeds the typical value (a fast eliminator).
eta.true <- c(-0.4, 0.20, 0.35)
D <- 320                                            # mg, single oral dose
round(c(KA = TH[[1]]*exp(eta.true[1]), V = TH[[2]]*exp(eta.true[2]),
        K  = TH[[3]]*exp(eta.true[3]),
        CL = TH[[2]]*exp(eta.true[2])*TH[[3]]*exp(eta.true[3]),
        t.half = log(2)/(TH[[3]]*exp(eta.true[3]))), 3)

set.seed(20260827)                                  # residual error (prop. + add.)
t.pt <- c(1, 2, 4, 8, 12)                           # sampling times (hr)
f.pt <- ipred(eta.true, t.pt, D)
dv.pt <- round(f.pt*(1 + rnorm(5, 0, sqrt(SG[["prop"]]))) +
               rnorm(5, 0, sqrt(SG[["add"]])), 2)
rbind(TIME = t.pt, DV = dv.pt)
