# 3-compartment: solution = sum of three exponentials. The 2-compartment characteristic
# equation was an eigenvalue problem; eigenvalues of K3 (negated) = alpha, beta, gamma
mic <- c(k10 = 0.30, k12 = 1.2, k21 = 0.5, k13 = 0.06, k31 = 0.015)   # /hr
K3  <- with(as.list(mic),
        matrix(c(-(k10 + k12 + k13), k21,  k31,
                 k12,               -k21,  0,
                 k13,                0,   -k31), 3, byrow = TRUE))
V.c <- 10; X0 <- c(300, 0, 0)                    # V1 = 10 L, D = 300 mg IV
e3  <- eigen(K3); W <- e3$vectors; Wi <- solve(W)
X3  <- function(t) W %*% (exp(e3$values*t)*(Wi %*% X0))   # amount in each compartment
C3iv <- function(t) sapply(t, function(s) X3(s)[1])/V.c

th3 <- sort(-log(2)/e3$values)                   # half-lives of the three phases (hr)
round(c(t.half.alpha = th3[1], t.half.beta = th3[2], t.half.gamma = th3[3]), 2)
co <- W[1, ]*(Wi %*% X0)[, 1]/V.c                # intercept of each exponential (mg/L)
cs <- sort(co, decreasing = TRUE)
round(c(A = cs[1], B = cs[2], G = cs[3]), 3)
# Fraction of body drug in the deep compartment (3): grows toward the terminal phase
round(setNames(sapply(c(1, 24, 96), function(s) {x <- X3(s); x[3]/sum(x)}),
               c("t=1", "t=24", "t=96")), 3)

t <- 10^seq(log10(0.05), log10(120), 0.01)
plot(t, C3iv(t), type = "l", log = "y", las = 1, bty = "l", yaxt = "n",
     xlim = c(0, 120), ylim = c(0.005, 40),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
axis(2, at = c(0.01, 0.1, 1, 10), labels = c("0.01", "0.1", "1", "10"), las = 1)
i2 <- order(e3$values)[1:2]                      # two fast terms (alpha, beta)
lines(t, co[i2[1]]*exp(e3$values[i2[1]]*t) +
         co[i2[2]]*exp(e3$values[i2[2]]*t), lty = 2)
abline(h = c(1, 0.02), lty = 3)
text(c(113, 113), c(1.45, 0.029), c("LLOQ A", "LLOQ B"), cex = 0.75)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("3-compartment", "fast two terms only"))
