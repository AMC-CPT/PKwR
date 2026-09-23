# Dedicated tools: base R nls and the author's wnl package
n1 <- nls(DV ~ A*exp(-k*x), d4, start = c(A = 50, k = 0.1))
round(summary(n1)$coefficients, 4)            # equal weights (OLS) by default

library(wnl)                                  # the error model is specified
fx <- function(TH) TH[1]*exp(-TH[2]*d4$x)
r4 <- nlr(fx, d4, pNames = c("A", "k"), IE = c(50, 0.1), Error = "P",
          LB = c(1, 0.01), UB = c(1e3, 5),
          SecNames = c("half.life"), SecForms = c(~log(2)/k))
round(r4$Est, 4)
