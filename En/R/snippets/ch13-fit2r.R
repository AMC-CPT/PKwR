# Hypothetical patient: 1000 mg as a 1-h infusion, twice 12 h apart; true CL is high.
etaT <- c(0.45, -0.15, 0, 0.20)
Dpt  <- addStop(data.frame(TIME = c(0, 12), AMT = 1000, RATE = 1000, DV = NA))
ts   <- c(13.5, 17, 23.5)                         # near peak, middle, trough
Dpt  <- rbind(Dpt, data.frame(TIME = ts, AMT = 0, RATE = 0, DV = NA))
Dpt  <- Dpt[order(Dpt$TIME), ]; ix <- match(ts, Dpt$TIME)
set.seed(20260828)
Dpt$DV[ix] <- round(pred2c(THv, etaT, Dpt, 90)[ix]*
                    (1 + rnorm(3, 0, sqrt(SGv[["prop"]]))), 2)
rbind(TIME = Dpt$TIME, RATE = Dpt$RATE, DV = Dpt$DV)

library(numDeriv)
D2p <- Dpt; D2p$DV[ix[2]] <- NA                   # only 2 points (peak, trough)
pkOf <- function(THv, eta, CLcr) {
  CL <- THv[1]*min(CLcr, 150)/100*exp(eta[1])
  c(CL = CL, V1 = THv[2]*exp(eta[2]), Q = THv[4]*exp(eta[4]),
    t.half = log(2)*THv[2]*exp(eta[2])/CL) }
f2 <- fitEBE(D2p, THv, OMv, 90); f3 <- fitEBE(Dpt, THv, OMv, 90)
round(rbind(prior = pkOf(THv, rep(0, 4), 90), MAP.2pt = pkOf(THv, f2$eta, 90),
            MAP.3pt = pkOf(THv, f3$eta, 90), truth = pkOf(THv, etaT, 90)), 3)
round(rbind(prior.SD = sqrt(diag(OMv)), post.SD.2pt = sqrt(diag(f2$COV)),
            post.SD.3pt = sqrt(diag(f3$COV))), 4)
