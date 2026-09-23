# 입력값 만들기 (1) Kp 예측: 조직을 물·중성지질·인지질의 혼합물로 보고
# 약물의 지질친화도와 결합만으로 분배계수를 계산한다 (Poulin-Theil).
comp <- read.table(header = TRUE, text = "
org     Vw     Vnl     Vph
LU   0.811   0.003   0.0090
AD   0.180   0.790   0.0020
BO   0.440   0.074   0.0011
BR   0.770   0.051   0.0565
HT   0.760   0.014   0.0111
KI   0.783   0.012   0.0242
MU   0.760   0.010   0.0090
SK   0.718   0.060   0.0044
SP   0.788   0.008   0.0136
GU   0.718   0.049   0.0141
LI   0.751   0.012   0.0240
PL   0.945   0.0035  0.0023")         # PL: 혈장
rownames(comp) <- comp$org
pl <- comp["PL", ]

kpPT <- function(o, logP, fup) {      # Poulin-Theil
  t <- comp[o, ]; P <- 10^logP
  fut <- 1/(1 + (1 - fup)/fup*0.5)    # 조직 유리분율의 통상 근사
  (P*(t$Vnl + 0.3*t$Vph) + (t$Vw + 0.7*t$Vph)) /
  (P*(pl$Vnl + 0.3*pl$Vph) + (pl$Vw + 0.7*pl$Vph)) * fup/fut
}

logP <- 2.5                           # 중등도 지용성
Kp.pred <- sapply(names(Kp), kpPT, logP = logP, fup = fu)
round(rbind(predicted = Kp.pred, assumed = Kp, ratio = Kp.pred/Kp), 2)

# 비지방 조직은 통상 말하는 2~3배 안이지만 지방은 자릿수가 다르다
nonad <- setdiff(names(Kp), "AD")
round(c(nonadipose.min = min(Kp.pred[nonad]/Kp[nonad]),
        nonadipose.max = max(Kp.pred[nonad]/Kp[nonad]),
        adipose = Kp.pred[["AD"]]/Kp[["AD"]]), 2)

# Vss 로 옮기면 그 오차가 그대로 드러난다 (식 10.7)
vss <- function(kp) V[["ART"]] + V[["VEN"]] + sum(kp*V[names(kp)])
round(c(Vss.pred = vss(Kp.pred), Vss.assumed = vss(Kp),
        ratio = vss(Kp.pred)/vss(Kp)), 2)

# logP 를 0.5 만 흔들어도 예측이 몇 배로 움직인다
round(sapply(c(1.5, 2.0, 2.5, 3.0), function(x)
        c(logP = x, Kp.AD = kpPT("AD", x, fu), Kp.MU = kpPT("MU", x, fu),
          Vss = vss(sapply(names(Kp), kpPT, logP = x, fup = fu)))), 1)

# middle-out: 임상 Vss 가 알려지면 예측 Kp 를 한 인자로 되맞춘다
f.mo <- (vss(Kp) - V[["ART"]] - V[["VEN"]])/(vss(Kp.pred) - V[["ART"]] - V[["VEN"]])
round(c(scale.factor = f.mo, Vss.after = vss(f.mo*Kp.pred)), 3)

# 입력값 만들기 (2) IVIVE: 시험관의 내적 청소율을 사람 간 전체로 올린다
MPPGL <- 40; LW <- 1800               # mg microsome 단백/g 간, 간 무게 g
scale <- MPPGL*LW*60/1e6              # uL/min/mg -> L/hr 로 가는 인자
ivive <- function(clv, fu.inc = 1) clv/fu.inc*scale
round(c(scale.factor = scale, vitro.needed = CLint/scale), 2)

# 배양액 결합(fu,inc)을 보정하지 않으면 CLint 를 그만큼 과소평가한다
round(setNames(sapply(c(1, 0.7, 0.5, 0.3), function(f) ivive(CLint/scale, f)),
               paste0("fu.inc=", c(1, 0.7, 0.5, 0.3))), 1)

# 두 단계의 오차가 곱해진 결과: CLint 가 2배 틀리면 경구 노출은 2배 틀린다
er <- function(cli) fu*cli/(QH + fu*cli)
m  <- c(0.5, 1, 2)*CLint
cl <- QH*er(m) + fu*GFR
round(rbind(CLint = m, ER = er(m), CL = cl, F.po = Fa*(1 - er(m)),
            AUC.po.rel = (Fa*(1 - er(m))/cl)/(Fa*(1 - er(CLint))/cl[2])), 4)
