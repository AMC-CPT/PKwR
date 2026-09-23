# From primary to secondary parameters: ke = CL/V,  t(1/2) = log(2)/ke
sec.par <- function(CL, V) c(CL = CL, V = V, ke = CL/V, t.half = log(2)*V/CL)

# The half-life does not change when CL and V change together in the same direction.
rbind(basal   = sec.par(CL = 5,   V = 50),
      CL.half = sec.par(CL = 2.5, V = 50),   # CL halved only -> half-life doubles
      V.double= sec.par(CL = 5,   V = 100),  # V doubled only -> half-life doubles
      both    = sec.par(CL = 2.5, V = 25))   # both halved -> half-life unchanged
