# Schild 분석: 길항이 정말 경쟁적인지, 길항제의 KB 가 얼마인지를
# 효능제 곡선의 '이동량'만으로 판정한다. 결합은 재지 않고 효과만 잰다.
set.seed(20260829)
KB <- 3e-8                                     # 길항제 참 해리상수 (M)
KA <- 5e-8                                     # 효능제 겉보기 EC50 (M)
B  <- KB*c(1, 3, 10, 30, 100)                  # 길항제 농도 (M)
CA <- 10^seq(-9, -4, 0.25)                     # 효능제 농도 (M)

# (1) 길항제 농도마다 효능제 곡선을 관측한다 (측정오차 SD 2%)
simE <- function(dr) Emax.model(CA, 100, KA*dr) + rnorm(length(CA), 0, 2)

# (2) 곡선마다 Emax 모형을 적합해 겉보기 EC50 을 얻는다
ec50 <- function(y) coef(nls(y ~ Em*CA/(E50 + CA),
                             start = c(Em = 100, E50 = KA)))[["E50"]]

# 경쟁적 길항: 용량비 DR = 1 + B/KB
dr.c <- 1 + B/KB
e0   <- ec50(simE(1))
DR.c <- sapply(dr.c, function(d) ec50(simE(d)))/e0

# 알로스테릭 음성 조절: DR = (1 + B/KB)/(1 + alp*B/KB), 천장은 1/alp
alp  <- 0.02
dr.a <- (1 + B/KB)/(1 + alp*B/KB)
DR.a <- sapply(dr.a, function(d) ec50(simE(d)))/e0
round(rbind(B.over.KB = B/KB, competitive = DR.c, allosteric = DR.a), 2)

# (3) Schild 회귀: log10(DR - 1) = log10(B) - log10(KB)
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

# (4) 한 농도만 써서 pA2 를 읽으면 알로스테릭 길항에서 KB 를 과대평가한다
onept <- function(DR, b) -log10(b/(DR - 1))
round(rbind(competitive = onept(DR.c, B), allosteric = onept(DR.a, B)), 2)
