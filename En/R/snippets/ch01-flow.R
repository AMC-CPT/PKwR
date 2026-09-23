# Conditions: if for a single value, ifelse to select over a whole vector at once
tlag <- 1; tt <- c(0, 0.5, 1, 2, 4)
round(ifelse(tt < tlag, 0, decay(tt - tlag)), 3)   # absorption with a lag time

# For a computation repeated until a condition is met: while and break
half <- function(k, tol = 1e-10) {            # find log(2)/k by bisection
  lo <- 0; hi <- 100
  repeat {
    mid <- (lo + hi)/2
    if (hi - lo < tol) break
    if (exp(-k*mid) > 0.5) lo <- mid else hi <- mid
  }
  mid
}
c(bisection = half(0.25), exact = log(2)/0.25)

# Write assumptions as code, not as comments. If one is wrong, it stops right there.
chk <- function(d) { stopifnot(!is.unsorted(d$ID), all(d$TIME >= 0)); "OK" }
chk(data.frame(ID = c(1, 1, 2), TIME = c(0, 2, 0)))
cat(try(chk(data.frame(ID = c(2, 1), TIME = c(0, 0))), silent = TRUE))
