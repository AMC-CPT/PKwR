# Approach to steady state during constant infusion: fraction of Css after n half-lives
n.half <- c(1, 2, 3, 4, 5, 6, 7)
data.frame(half.lives = n.half,
           pct.of.Css = round(100*(1 - 0.5^n.half), 2))
