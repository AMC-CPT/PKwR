# 정량한계 미만(BLQ)을 어떻게 다룰 것인가. 48시간까지 채혈한 자료를 만들고
# LLOQ = 1 mg/L 로 잘라 본다. (개인간 변이는 통합 적합이 흡수하므로 여기서
# 얻는 것은 참값과의 거리가 아니라 '자르지 않은 적합'과의 거리다.)
set.seed(20260828)
lloq <- 1.0
tt   <- c(0.25, 0.5, 1, 2, 4, 7, 9, 12, 24, 36, 48)
sim  <- do.call(rbind, lapply(1:12, function(i) {
  et <- drop(t(chol(OM)) %*% rnorm(3))            # OMEGA 를 따르는 개인 편차
  f  <- PRED(TH, et, cbind(TIME = tt, DOSE = 320))[, "F"]
  data.frame(ID = i, TIME = tt, DOSE = 320,
             DV = f + f*rnorm(length(tt), 0, sqrt(SG[1,1])) +
                      rnorm(length(tt), 0, sqrt(SG[2,2])))
}))
c(n = nrow(sim), n.blq = sum(sim$DV < lloq), pct.blq = round(100*mean(sim$DV < lloq), 1))
round(tapply(sim$DV < lloq, sim$TIME, mean), 3)    # 시각별 BLQ 비율

# 세 가지 처리를 목적함수 하나로 표현한다. 다른 것은 두 줄뿐이다.
Fpop <- function(th, dose, t)
  dose/th[2]*th[1]/(th[1] - th[3])*(exp(-th[3]*t) - exp(-th[1]*t))

m2ll <- function(p, d, how) {
  th <- exp(p[1:3]); sg <- exp(p[4:5])
  f  <- Fpop(th, d$DOSE, d$TIME); v <- sg[1]*f^2 + sg[2]
  y  <- d$DV; b <- y < lloq                        # 검열된 관측
  if (how == "full") b[] <- FALSE                  # 자르지 않은 자료 (기준)
  if (how == "M5") { y[b] <- lloq/2; b[] <- FALSE } # LLOQ/2 로 대치
  o <- sum(log(v[!b]) + (y[!b] - f[!b])^2/v[!b])   # M1 은 여기서 끝(버린다)
  if (how == "M3" && any(b))                       # M3: P(Y < LLOQ) 를 더한다
    o <- o - 2*sum(pnorm(lloq, f[b], sqrt(v[b]), log.p = TRUE))
  o
}
p0  <- log(c(TH, SG[1,1], SG[2,2]))
fit <- function(how) exp(optim(p0, m2ll, d = sim, how = how,
                               control = list(maxit = 3000, reltol = 1e-12))$par)
res <- sapply(c("full", "M1", "M5", "M3"), fit)
rownames(res) <- c("ka", "V", "ke", "sg.prop", "sg.add")
res <- rbind(res, CL = res["V", ]*res["ke", ], t.half = log(2)/res["ke", ])
round(res, 4)
round(100*(res[, c("M1", "M5", "M3")]/res[, "full"] - 1), 2)   # full 대비 편향 %

# 그림: 꼬리를 버리거나 상수로 채우면 말기 기울기가 눕는다
tg <- seq(0.05, 48, 0.05)
plot(sim$TIME, pmax(sim$DV, 0.06), log = "y", las = 1, bty = "n", cex = 0.6,
     pch = ifelse(sim$DV < lloq, 1, 16), col = ifelse(sim$DV < lloq, "grey60", "grey25"),
     xlab = "시각 (hr)", ylab = "농도 (mg/L)", ylim = c(0.06, 20))
abline(h = lloq, lty = 3)
text(46, lloq*1.25, "LLOQ", cex = 0.8, adj = 1)
for (j in seq_len(4)) lines(tg, Fpop(res[1:3, j], 320, tg), lwd = 2,
                            lty = c(1, 2, 4, 1)[j], col = c(1, 2, 4, 3)[j])
legend("bottomleft", bty = "n", cex = 0.85, lwd = 2, lty = c(1, 2, 4, 1),
       col = c(1, 2, 4, 3), legend = c("자르지 않은 자료", "M1 (버린다)",
       "M5 (LLOQ/2)", "M3 (우도에 넣는다)"))
