# Check the closed form by running the engine to steady state (a habit of this book).
chkSS <- function(nm, ndose = 12) {
  m <- MODELS[[nm]]; d <- doseFor(m, rep(0, 4), 90)[["dose"]]
  Dv <- addStop(data.frame(TIME = (0:(ndose - 1))*m$tau, AMT = d,
                           RATE = d/m$Tinf, DV = NA))
  Dg <- data.frame(TIME = sort(unique(c(Dv$TIME, seq(0, ndose*m$tau, 0.05)))),
                   AMT = 0, DV = NA)
  Dg$RATE <- approx(Dv$TIME, Dv$RATE, Dg$TIME, method = "constant",
                    rule = 2, f = 0)$y
  cc <- pred2c(m$TH, rep(0, 4), Dg, 90)
  lastI <- Dg$TIME >= (ndose - 1)*m$tau
  cl <- doseFor(m, rep(0, 4), 90)
  c(Cmax.formula = cl[["Cmax"]], Cmax.engine = max(cc[lastI]),
    Cmin.formula = cl[["Cmin"]], Cmin.engine = min(cc[lastI])) }
round(t(sapply(names(MODELS), chkSS, ndose = 12)), 3)
round(t(sapply(names(MODELS), chkSS, ndose = 40))[, c(2, 4)], 3)   # with longer dosing
