# Building the inputs (1) Kp prediction: the tissue is a mixture of water, neutral
# lipid and phospholipid; lipophilicity and binding alone give Kp (Poulin-Theil).
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
PL   0.945   0.0035  0.0023")         # PL: plasma
rownames(comp) <- comp$org
pl <- comp["PL", ]

kpPT <- function(o, logP, fup) {      # Poulin-Theil
  t <- comp[o, ]; P <- 10^logP
  fut <- 1/(1 + (1 - fup)/fup*0.5)    # usual approximation to tissue unbound fraction
  (P*(t$Vnl + 0.3*t$Vph) + (t$Vw + 0.7*t$Vph)) /
  (P*(pl$Vnl + 0.3*pl$Vph) + (pl$Vw + 0.7*pl$Vph)) * fup/fut
}

logP <- 2.5                           # moderately lipophilic
Kp.pred <- sapply(names(Kp), kpPT, logP = logP, fup = fu)
round(rbind(predicted = Kp.pred, assumed = Kp, ratio = Kp.pred/Kp), 2)

# non-adipose tissues are within the usual 2-3 fold, adipose by an order of magnitude
nonad <- setdiff(names(Kp), "AD")
round(c(nonadipose.min = min(Kp.pred[nonad]/Kp[nonad]),
        nonadipose.max = max(Kp.pred[nonad]/Kp[nonad]),
        adipose = Kp.pred[["AD"]]/Kp[["AD"]]), 2)

# carried into Vss, that error shows up as it is (Eq. 10.7)
vss <- function(kp) V[["ART"]] + V[["VEN"]] + sum(kp*V[names(kp)])
round(c(Vss.pred = vss(Kp.pred), Vss.assumed = vss(Kp),
        ratio = vss(Kp.pred)/vss(Kp)), 2)

# shifting logP by only 0.5 moves the prediction several-fold
round(sapply(c(1.5, 2.0, 2.5, 3.0), function(x)
        c(logP = x, Kp.AD = kpPT("AD", x, fu), Kp.MU = kpPT("MU", x, fu),
          Vss = vss(sapply(names(Kp), kpPT, logP = x, fup = fu)))), 1)

# middle-out: once a clinical Vss is known, rescale the predicted Kp by one factor
f.mo <- (vss(Kp) - V[["ART"]] - V[["VEN"]])/(vss(Kp.pred) - V[["ART"]] - V[["VEN"]])
round(c(scale.factor = f.mo, Vss.after = vss(f.mo*Kp.pred)), 3)

# Building the inputs (2) IVIVE: scale the in vitro intrinsic CL to the whole liver
MPPGL <- 40; LW <- 1800               # mg microsomal protein/g liver, liver weight g
scale <- MPPGL*LW*60/1e6              # factor taking uL/min/mg -> L/hr
ivive <- function(clv, fu.inc = 1) clv/fu.inc*scale
round(c(scale.factor = scale, vitro.needed = CLint/scale), 2)

# uncorrected incubation binding (fu,inc) underestimates CLint by the same factor
round(setNames(sapply(c(1, 0.7, 0.5, 0.3), function(f) ivive(CLint/scale, f)),
               paste0("fu.inc=", c(1, 0.7, 0.5, 0.3))), 1)

# the two errors multiply: a 2-fold error in CLint is a 2-fold error in oral exposure
er <- function(cli) fu*cli/(QH + fu*cli)
m  <- c(0.5, 1, 2)*CLint
cl <- QH*er(m) + fu*GFR
round(rbind(CLint = m, ER = er(m), CL = cl, F.po = Fa*(1 - er(m)),
            AUC.po.rel = (Fa*(1 - er(m))/cl)/(Fa*(1 - er(CLint))/cl[2])), 4)
