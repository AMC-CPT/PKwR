# Black-Leff 조작 모형: 점유(Kd)와 변환(tau)을 분리해서 쓴다.
# tau = Rtot/KE 는 '이 조직이 점유를 반응으로 얼마나 증폭하는가'이다.
oper <- function(C, Em, Kd, tau) Em*tau*C/(Kd + (1 + tau)*C)

# 관측되는 두 값은 tau 의 함수로 닫힌 꼴이 있다
tau  <- c(0.1, 0.3, 1, 3, 10, 30)
Kd   <- 100                                    # nM, 모든 열에서 같다
round(rbind(tau         = tau,
            Emax.obs    = 100*tau/(1 + tau),   # Em = 100 기준
            EC50        = Kd/(1 + tau),
            EC50.over.Kd = 1/(1 + tau),
            occ.at.EC50 = 1/(2 + tau)), 3)     # EC50 에서의 점유율

# 같은 Kd 라도 tau 가 크면 완전효능제(높고 왼쪽), 작으면 부분효능제(낮고 오른쪽)
CC <- 10^seq(0, 4, 0.01)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CC, oper(CC, 100, Kd, tau[1]), type = "n", log = "x", las = 1, bty = "l",
     ylim = c(0, 108), xlab = "Agonist (nM)", ylab = "Effect",
     main = "(a) one Kd, six tau")
for (i in seq_along(tau)) lines(CC, oper(CC, 100, Kd, tau[i]),
                                lty = if (i %% 2) 1 else 2)
abline(v = Kd, lty = 3)
text(Kd, 5, "Kd", cex = 0.75, pos = 4)

# 곡선 하나로는 (Em, Kd, tau) 를 가릴 수 없다: 한 자유도가 남는다
p1   <- c(Em = 100, Kd = 100, tau = 3)
Ec   <- p1[["Kd"]]/(1 + p1[["tau"]])           # 관측 EC50
Eo   <- p1[["Em"]]*p1[["tau"]]/(1 + p1[["tau"]])  # 관측 Emax
t2   <- 9                                      # tau 를 3배로 키우고
p2   <- c(Em = Eo*(1 + t2)/t2,                 # Em 과 Kd 를 그에 맞춰 되돌리면
          Kd = Ec*(1 + t2), tau = t2)          # 곡선이 완전히 겹친다
round(p2, 3)
y1 <- oper(CC, p1[["Em"]], p1[["Kd"]], p1[["tau"]])
y2 <- oper(CC, p2[["Em"]], p2[["Kd"]], p2[["tau"]])
round(c(max.abs.diff  = max(abs(y1 - y2)),
        occ.at.EC50.1 = Ec/(p1[["Kd"]] + Ec),
        occ.at.EC50.2 = Ec/(p2[["Kd"]] + Ec)), 6)

# Emax 모형으로 적합하면 EC50 과 Emax 는 나오지만 Kd 도 tau 도 나오지 않는다
round(coef(nls(y1 ~ Emx*CC/(E50 + CC), start = c(Emx = 90, E50 = 30),
               control = nls.control(scaleOffset = 1))), 3)

# 부분효능제는 완전효능제와 함께 있으면 길항제처럼 행동한다
both <- function(A, P, Em = 100, KA = 100, KP = 100, tA = 10, tP = 0.5) {
  a <- A/KA; p <- P/KP
  Em*(tA*a + tP*p)/(1 + a + p + tA*a + tP*p)
}
Pv <- c(0, 100, 1000, 10000)
plot(CC, both(CC, 0), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 108), xlab = "Full agonist (nM)", ylab = "Effect",
     main = "(b) partial agonist added")
for (P in Pv[-1]) lines(CC, both(CC, P), lty = 2)
text(1.5, sapply(Pv, function(P) both(1, P)) + 4, labels = Pv, cex = 0.7)
legend("bottomright", "partial agonist (nM)", bty = "n", cex = 0.75)

# 완전효능제가 없을 때의 바닥과 아주 많을 때의 천장을 함께 본다
round(rbind(P = Pv,
            floor = sapply(Pv, function(P) both(0, P)),
            top   = sapply(Pv, function(P) both(1e5, P))), 2)
