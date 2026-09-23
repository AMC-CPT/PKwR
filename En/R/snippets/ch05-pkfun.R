# One-compartment concentration functions - used repeatedly in this chapter.
# IV bolus:  C(t) = (D/V) exp(-k t)
Civ <- function(t, D, V, k) D/V * exp(-k*t)

# Constant infusion:  C(t) = (R0/CL) (1 - exp(-k t)),  CL = V k
Cinf <- function(t, R0, V, k) R0/(V*k) * (1 - exp(-k*t))

# Oral, first-order absorption: C(t) = (F D/V) ka/(ka-k) (exp(-k t) - exp(-ka t))
Cpo <- function(t, D, V, k, ka, F = 1)
  F*D/V * ka/(ka - k) * (exp(-k*t) - exp(-ka*t))

# Conversion between half-life and elimination rate constant
k2half <- function(k) log(2)/k
half2k <- function(t.half) log(2)/t.half

c(k = half2k(12), t.half = k2half(half2k(12)))
