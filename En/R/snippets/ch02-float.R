# Computer reals occupy finite bits; a finite decimal may be an infinite binary fraction
c(0.1 + 0.2 == 0.3, 99/(72*1.3) == 99/72/1.3)  # Cockcroft-Gault: only the order differs
signif(c(gap = 0.1 + 0.2 - 0.3), 4)
isTRUE(all.equal(99/(72*1.3), 99/72/1.3))     # compare with a tolerance, not equality

# Machine epsilon: the smallest number that, added to 1, is distinguishable from 1
macheps <- function(x = 1) { e <- 1; while (x + e > x) e <- e/2; 2*e }
signif(c(mine = macheps(), R = .Machine$double.eps, at100 = macheps(100)), 4)

# Overflow and underflow are not errors; they silently become Inf and 0
signif(c(exp709 = exp(709), exp710 = exp(710), exp.m745 = exp(-745),
         exp.m746 = exp(-746)), 4)
signif(c(left = 1e200*1e300*1e-200, right = 1e200*(1e300*1e-200)), 4)

# Subtracting two close numbers loses significant digits (absorption model, ka near k)
ka1 <- 0.2500001; ke1 <- 0.25
signif(c(difference = ka1 - ke1, rel.error = abs((ka1 - ke1)/1e-7 - 1)), 4)
