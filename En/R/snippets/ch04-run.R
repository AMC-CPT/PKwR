# Runs test: are the signs of the residuals randomly mixed? A wave means fewer runs.
run.p <- function(m, n, r) {                  # P(number of runs <= r), m <= n
  if (m < 1 || n < 1 || r < 2) return(0)
  f <- function(u) if (u %% 2 == 0) { k <- u/2
      2*choose(m - 1, k - 1)*choose(n - 1, k - 1)
    } else { k <- (u + 1)/2
      choose(m - 1, k - 1)*choose(n - 1, k - 2) +
      choose(m - 1, k - 2)*choose(n - 1, k - 1) }
  sum(sapply(2:r, f))/choose(m + n, m) }

runtest <- function(res) {                    # observed runs and exact-test p value
  sg <- res[res != 0] > 0; nt <- length(sg)
  nr <- sum(sg[-1] != sg[-nt]) + 1            # number of sign changes + 1
  m <- min(sum(sg), nt - sum(sg)); n <- nt - m
  p <- run.p(m, n, nr); if (p > 0.5) p <- 1 - run.p(m, n, nr - 1)
  c(n = nt, runs = nr, expected = 2*m*n/nt + 1, p = p) }

round(rbind(correct = runtest(d4$DV - f.els),      # (b) correct model
            misspecified = runtest(y2 - f.mis)), 4) # (c) one-exp fit to two-exp data
