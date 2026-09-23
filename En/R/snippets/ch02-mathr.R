# Re-check a few calculations of this chapter with the author's mathr package
library(mathr)
signif(c(MachEps = MachEps(), R = .Machine$double.eps), 4)   # 2.1 machine epsilon

# The integral and derivative of 2.5 (g = A exp(-k t) as above, exactI = analytic value)
print(c(exact = exactI, GQuad8 = GQuad8(g, 0, 20),          # 8-point Gauss quadrature
        Romberg = romb(g, 0, 20, N = 8)), digits = 7)
print(c(Deriv1 = Deriv1(g, 4), by.hand = -k*A*exp(-k*4)), digits = 7)

# Minimize the Rosenbrock function of 2.5 by the variable metric method
vm <- VMmin(c(-1.2, 1), ros)
signif(c(x = vm$par[1], y = vm$par[2], value = vm$value), 4); vm$FnCount

# Cholesky-factor M of 2.4 (unlike R's chol, it returns a lower triangular matrix)
round(Chol(M), 4)
