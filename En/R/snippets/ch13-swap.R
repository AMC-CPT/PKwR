# New drug: swap only model list and target; engine (addStop, pred2c, fitEBE) stays.
# Illustrative values of literature magnitude; in practice use your own NONMEM output.
MODELS <- list(
  vancomycin = list(TH = c(3.96, 33.1, 48.3, 6.99), tau = 12, Tinf = 1.0,
                    tgt = list(kind = "AUC24", value = 410)),
  gentamicin = list(TH = c(5.40, 14.0, 12.0, 1.50), tau = 24, Tinf = 0.5,
                    tgt = list(kind = "Cmax",  value = 20)),
  amikacin   = list(TH = c(5.00, 18.0, 14.0, 1.50), tau = 24, Tinf = 0.5,
                    tgt = list(kind = "Cmax",  value = 60)))

pk4 <- function(TH, eta, CLcr) list(CL = TH[1]*min(CLcr, 150)/100*exp(eta[1]),
  V1 = TH[2]*exp(eta[2]), V2 = TH[3]*exp(eta[3]), Q = TH[4]*exp(eta[4]))
ssM <- function(pk, Dose, tau, Tinf) with(pk, {   # 2-cmt infusions at steady state
  k10 <- CL/V1; k12 <- Q/V1; k21 <- Q/V2
  S <- k10 + k12 + k21; be <- (S - sqrt(S*S - 4*k21*k10))/2; al <- k21*k10/be
  A <- (al - k21)/(al - be)/V1/al; B <- (be - k21)/(be - al)/V1/be
  eAT <- exp(al*tau); eAD <- exp(al*Tinf); eBT <- exp(be*tau); eBD <- exp(be*Tinf)
  c(AUC24 = Dose/CL*24/tau,
    Cmin = Dose/Tinf*(A*(eAD - 1)/(eAT - 1) + B*(eBD - 1)/(eBT - 1)),
    Cmax = Dose/Tinf*(A*(1 - 1/eAD)/(1 - 1/eAT) + B*(1 - 1/eBD)/(1 - 1/eBT))) })

doseFor <- function(m, eta, CLcr) {               # single dose that meets the target
  pk <- pk4(m$TH, eta, CLcr)
  d <- if (m$tgt$kind == "AUC24") m$tgt$value*pk$CL*m$tau/24 else
       uniroot(function(x) ssM(pk, x, m$tau, m$Tinf)[["Cmax"]] - m$tgt$value,
               c(1, 1e4))$root
  d <- round(d/10)*10
  c(CL = pk$CL, tau = m$tau, dose = d, ssM(pk, d, m$tau, m$Tinf)) }
round(t(sapply(MODELS, doseFor, eta = rep(0, 4), CLcr = 90)), 2)
