# 런 검정: 잔차의 부호가 무작위로 섞여 있는가. 파형이 있으면 런의 수가 적어진다.
run.p <- function(m, n, r) {                  # P(런 수 <= r), m <= n
  if (m < 1 || n < 1 || r < 2) return(0)
  f <- function(u) if (u %% 2 == 0) { k <- u/2
      2*choose(m - 1, k - 1)*choose(n - 1, k - 1)
    } else { k <- (u + 1)/2
      choose(m - 1, k - 1)*choose(n - 1, k - 2) +
      choose(m - 1, k - 2)*choose(n - 1, k - 1) }
  sum(sapply(2:r, f))/choose(m + n, m) }

runtest <- function(res) {                    # 관측 런 수와 정확검정 p 값
  sg <- res[res != 0] > 0; nt <- length(sg)
  nr <- sum(sg[-1] != sg[-nt]) + 1            # 부호가 바뀐 횟수 + 1
  m <- min(sum(sg), nt - sum(sg)); n <- nt - m
  p <- run.p(m, n, nr); if (p > 0.5) p <- 1 - run.p(m, n, nr - 1)
  c(n = nt, runs = nr, expected = 2*m*n/nt + 1, p = p) }

round(rbind(correct = runtest(d4$DV - f.els),      # (b) 옳은 모형
            misspecified = runtest(y2 - f.mis)), 4) # (c) 2지수 자료에 1지수 적합
