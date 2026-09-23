# Schild analysis: judges from the 'shift' of the agonist curve alone whether the
# antagonism is truly competitive and what its KB is. Effect is measured, not binding.
set.seed(20260829)
KB <- 3e-8                                     # true KB of the antagonist (M)
KA <- 5e-8                                     # apparent EC50 of the agonist (M)
B  <- KB*c(1, 3, 10, 30, 100)                  # antagonist concentrations (M)
CA <- 10^seq(-9, -4, 0.25)                     # agonist concentrations (M)

# (1) Observe the agonist curve at each antagonist concentration (meas. error SD 2%)
simE <- function(dr) Emax.model(CA, 100, KA*dr) + rnorm(length(CA), 0, 2)

# (2) Fit an Emax model to each curve to obtain the apparent EC50
ec50 <- function(y) coef(nls(y ~ Em*CA/(E50 + CA),
                             start = c(Em = 100, E50 = KA)))[["E50"]]

# Competitive antagonism: dose ratio DR = 1 + B/KB
dr.c <- 1 + B/KB
e0   <- ec50(simE(1))
DR.c <- sapply(dr.c, function(d) ec50(simE(d)))/e0

# Negative allosteric modulation: DR = (1 + B/KB)/(1 + alp*B/KB), ceiling 1/alp
alp  <- 0.02
dr.a <- (1 + B/KB)/(1 + alp*B/KB)
DR.a <- sapply(dr.a, function(d) ec50(simE(d)))/e0
round(rbind(B.over.KB = B/KB, competitive = DR.c, allosteric = DR.a), 2)

# (3) Schild regression: log10(DR - 1) = log10(B) - log10(KB)
schild <- function(DR) {
  f <- lm(log10(DR - 1) ~ log10(B))
  c(slope = coef(f)[[2]], pA2 = coef(f)[[1]]/coef(f)[[2]])
}
round(rbind(competitive = schild(DR.c), allosteric = schild(DR.a),
            true = c(1, -log10(KB))), 3)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CA, Emax.model(CA, 100, KA), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 108), xlab = "Agonist (M)", ylab = "Effect",
     main = "(a) competitive shift")
for (d in dr.c) lines(CA, Emax.model(CA, 100, KA*d), lty = 2)
abline(h = 50, lty = 3)

xb <- log10(B)
plot(xb, log10(DR.c - 1), pch = 16, las = 1, bty = "l", ylim = c(-0.6, 2.2),
     xlab = "log10 [B]", ylab = "log10 (DR - 1)", main = "(b) Schild plot")
abline(lm(log10(DR.c - 1) ~ xb))
points(xb, log10(DR.a - 1), pch = 1)
lines(xb, log10(dr.a - 1), lty = 2)
abline(h = log10(1/alp - 1), lty = 3)
text(min(xb), log10(1/alp - 1) + 0.16, "ceiling = 1/alpha", cex = 0.75, pos = 4)
legend("bottomright", c("competitive", "allosteric"), pch = c(16, 1),
       bty = "n", cex = 0.8)

# (4) pA2 from one concentration overestimates KB under allosteric antagonism
onept <- function(DR, b) -log10(b/(DR - 1))
round(rbind(competitive = onept(DR.c, B), allosteric = onept(DR.a, B)), 2)
