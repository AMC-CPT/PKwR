# 재표집 1: bootstrap. 분포를 가정하지 않고 자료를 되뽑아 불확실성을 잰다.
set.seed(20260828)
bs <- replicate(2000, gm(sample(x, length(x), replace = TRUE)))
round(c(gm.obs = gm(x), boot.SE = sd(bs),
        quantile(bs, c(0.025, 0.975)),
        wald.lo = exp(mean(log(x)) - 1.96*sd(log(x))/sqrt(length(x))),
        wald.hi = exp(mean(log(x)) + 1.96*sd(log(x))/sqrt(length(x)))), 3)

# 재표집 2: 순열검정. 라벨을 섞어 '우연이면 이만한 차이가 얼마나 나오는가'를 센다.
g  <- rep(c("A", "B"), each = 100)
obs <- mean(log(x)[g == "B"]) - mean(log(x)[g == "A"])
perm <- replicate(2000, { p <- sample(g)
  mean(log(x)[p == "B"]) - mean(log(x)[p == "A"]) })
round(c(obs.diff = obs, perm.p = mean(abs(perm) >= abs(obs)),
        t.p = t.test(log(x) ~ g)$p.value), 4)
