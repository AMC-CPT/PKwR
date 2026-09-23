# Root finding: x with f(x) = 0. uniroot is safe within a sign-changing interval.
Cinf <- function(t, R0 = 50, CL = 5, Vd = 30) R0/CL*(1 - exp(-CL/Vd*t))
f90  <- function(t) Cinf(t) - 0.9*50/5        # time to reach 90% of steady state
u <- uniroot(f90, c(0, 100), tol = 1e-10)
round(c(uniroot = u$root, exact = -log(0.1)*30/5, iter = u$iter), 4)

# Newton-Raphson: much faster convergence when the derivative is known (oral-model tmax)
gg  <- function(t) ka*exp(-ka*t) - ke*exp(-ke*t)          # dC/dt = 0
ggp <- function(t) -ka^2*exp(-ka*t) + ke^2*exp(-ke*t)
tn <- 1; path <- tn
for (i in 1:5) { tn <- tn - gg(tn)/ggp(tn); path <- c(path, tn) }
signif(path - log(ka/ke)/(ka - ke), 3)        # error: correct digits double per step
c(newton = tn, closed.form = log(ka/ke)/(ka - ke))
