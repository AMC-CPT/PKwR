# Minimization with and without the gradient. The Rosenbrock minimum is at (1, 1).
ros <- function(v) 100*(v[2] - v[1]^2)^2 + (1 - v[1])^2
nm <- optim(c(-1.2, 1), ros, method = "Nelder-Mead")
bf <- optim(c(-1.2, 1), ros, method = "BFGS")
signif(rbind(Nelder.Mead = c(nm$par, value = nm$value, fcalls = nm$counts[[1]]),
             BFGS        = c(bf$par, value = bf$value, fcalls = bf$counts[[1]])), 4)

# Supplying the gradient formula directly cuts the function calls greatly
grd <- function(v) c(-400*v[1]*(v[2] - v[1]^2) - 2*(1 - v[1]), 200*(v[2] - v[1]^2))
bg <- optim(c(-1.2, 1), ros, grd, method = "BFGS")
signif(c(x = bg$par[1], y = bg$par[2], value = bg$value,
         fcalls = bg$counts[[1]], gcalls = bg$counts[[2]]), 4)
