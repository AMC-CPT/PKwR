# Compare three quadrature rules on the same 11 points (g is A exp(-k t) from above)
tp <- seq(0, 20, length.out = 11); yp <- g(tp)
trapz <- function(t, y) sum(diff(t)*(head(y, -1) + tail(y, -1))/2)
simpson <- function(t, y) { n <- length(t) - 1; h <- diff(t)[1]
  h/3*(y[1] + 4*sum(y[seq(2, n, 2)]) + 2*sum(y[seq(3, n - 1, 2)]) + y[n + 1]) }
logtrap <- function(t, y) { i <- seq_len(length(t) - 1)   # log-trapezoidal
  sum((y[i] - y[i + 1])*diff(t)/log(y[i]/y[i + 1])) }

exactI <- A/k*(1 - exp(-k*20))
round(c(exact = exactI, trapezoid = trapz(tp, yp), simpson = simpson(tp, yp),
        log.trapezoid = logtrap(tp, yp)), 4)
signif(c(trap = trapz(tp, yp) - exactI, simpson = simpson(tp, yp) - exactI), 3)
signif(logtrap(tp, yp) - exactI, 3)           # exact for a pure exponential

# By what factor does the error fall when the points double (definition of the order)
e <- sapply(c(11, 21, 41), function(m) { tq <- seq(0, 20, length.out = m)
  c(trap = trapz(tq, g(tq)), simp = simpson(tq, g(tq))) - exactI })
round(rbind(ratio.trap = e[1, -3]/e[1, -1], ratio.simp = e[2, -3]/e[2, -1]), 2)
