# Growth-kill model: the drug kills proliferating cells, but death is observed only
# after transit compartments (anticancer: tumor cells; antimicrobial: bacterial count).
library(deSolve)
kg <- 0.03; kmax <- 0.45; EC50k <- 2; ktr <- 0.15    # /hr, mg/L
Cp.k <- function(t, D) D/10*exp(-0.12*t)             # 1-compartment IV, t1/2 5.8 hr
dkill <- function(t, y, p) {
  Ck <- Cp.k(t, p[["D"]])
  kk <- kmax*Ck/(EC50k + Ck)                         # concentration-driven kill rate
  c(list(c(kg*y[1] - kk*y[1],                        # proliferating compartment
           kk*y[1] - ktr*y[2], ktr*(y[2] - y[3]),    # kill transit chain
           ktr*(y[3] - y[4]))), total = sum(y))
}
tk <- seq(0, 240, 0.5); Dk <- c(0, 30, 100, 300)
Nk <- sapply(Dk, function(d)
        lsoda(c(100, 0, 0, 0), tk, dkill, c(D = d))[, "total"])

matplot(tk, Nk, type = "l", lty = 1:4, col = 1, log = "y", las = 1, bty = "l",
        xlab = "Time (hr)", ylab = "Cell number (% of initial)",
        ylim = c(1, 6000))
abline(h = 100, lty = 3)
legend("top", bty = "n", cex = 0.75, lty = 1:4, horiz = TRUE,
       legend = paste("D =", Dk))

# Stasis concentration: where kill equals proliferation and net growth becomes zero
Cst <- EC50k*kg/(kmax - kg)
res.k <- rbind(nadir.pct = apply(Nk, 2, min), t.nadir = tk[apply(Nk, 2, which.min)],
               C0.over.Cst = Dk/10/Cst)
colnames(res.k) <- paste("D =", Dk)
round(c(C.stasis = Cst), 3); round(res.k, 2)
