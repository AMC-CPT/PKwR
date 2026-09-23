# Analytic solution vs numerical methods: 100 mg bolus, two compartments, 12 h later.
# The reference is the analytic solution: one step, no intermediate time computed.
ref12 <- as.vector(expAt(12) %*% c(100, 0))
round(c(central = ref12[1], peripheral = ref12[2]), 6)

# (a) Fixed-step fourth-order Runge-Kutta. The fast-phase half-life is log(2)/lambda1 =
#     0.17 h, so a larger step diverges (a stability problem, not one of precision).
rk4 <- function(h) {
  y <- c(100, 0)
  for (i in seq_len(round(12/h))) {
    k1 <- Amt %*% y;            k2 <- Amt %*% (y + h/2*k1)
    k3 <- Amt %*% (y + h/2*k2); k4 <- Amt %*% (y + h*k3)
    y  <- y + h/6*(k1 + 2*k2 + 2*k3 + k4)
  }
  as.vector(y)
}
hs <- c(2, 1, 0.5, 0.25, 0.1)
data.frame(h = hs, steps = 12/hs,
           rel.error = signif(sapply(hs, function(h) rk4(h)[1]/ref12[1] - 1), 3))

# (b) lsoda chooses its own step. Loosen the tolerance and the result is off by as much.
tol <- 10^-c(2, 4, 6, 8, 10)
data.frame(tol = tol,
           rel.error = signif(sapply(tol, function(x)
             ode(c(100, 0), c(0, 12), function(t, y, p) list(Amt %*% y),
                 NULL, rtol = x, atol = x)[2, 2]/ref12[1] - 1), 3))
