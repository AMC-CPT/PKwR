# Where the denominator becomes 0: absorption rate constant close to elimination rate.
# The formula becomes 0/0; the limit is Xg0*K*t*exp(-K*t) (equation (eq:lap1clim)).
oral1 <- function(Xg0, Ke, Ka, t) Ka*Xg0/(Ka - Ke)*(exp(-Ke*t) - exp(-Ka*t))
lim1  <- function(Xg0, K,  t)     Xg0*K*t*exp(-K*t)

dd <- 10^-c(2, 4, 6, 8, 10, 12, 14, 16)
data.frame(delta = dd,
           formula = sapply(dd, function(x) oral1(100, 0.1, 0.1 + x, 4)),
           limit = lim1(100, 0.1, 4),
           rel.error = signif(sapply(dd, function(x)
             oral1(100, 0.1, 0.1 + x, 4)/lim1(100, 0.1, 4) - 1), 3))

# The two roots of the two-compartment model are always distinct real numbers (theorem),
# because the discriminant equals (K10 - K21)^2 + K12^2 + 2*K12*(K10 + K21).
disc <- function(p) (p[1] - p[3])^2 + p[2]^2 + 2*p[2]*(p[1] + p[3])
set.seed(8)
pr <- matrix(10^runif(30000, -3, 1), ncol = 3)     # random rate constants in 0.001~10
c(identity.err = max(abs(apply(pr, 1, function(x) disc(x) - (sum(x)^2 - 4*x[1]*x[3])))),
  min.discriminant = min(apply(pr, 1, disc)),
  min.root.gap = min(sqrt(apply(pr, 1, disc))))
