# Numerical integration: trapezoids join adjacent points by lines; sum the areas beneath
g  <- function(t) A*exp(-k*t)
trap <- function(t) { y <- g(t); sum(diff(t)*(head(y, -1) + tail(y, -1))/2) }
exactI <- A/k*(1 - exp(-k*20))                # analytic solution
round(c(exact = exactI,
        n5  = trap(seq(0, 20, length.out = 5)),
        n21 = trap(seq(0, 20, length.out = 21)),
        n81 = trap(seq(0, 20, length.out = 81)),
        integrate = integrate(g, 0, 20)$value), 4)   # R's adaptive quadrature
