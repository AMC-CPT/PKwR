# Forward model (2-cmt IV): advance any dosing history by state transitions per interval
# Rate must be constant within an interval, so 'infusion stops' are added as events.
addStop <- function(Dv) {
  s <- Dv[Dv$RATE > 0, , drop = FALSE]
  if (nrow(s)) { s$TIME <- s$TIME + s$AMT/s$RATE; s$AMT <- 0; s$RATE <- 0; s$DV <- NA }
  Dv <- rbind(Dv, s); Dv[order(Dv$TIME), ] }

pred2c <- function(THv, eta, Dv, CLcr) {
  CL <- THv[1]*min(CLcr, 150)/100*exp(eta[1]); V1 <- THv[2]*exp(eta[2])
  V2 <- THv[3]*exp(eta[3]);                    Qi <- THv[4]*exp(eta[4])
  ke <- CL/V1; k12 <- Qi/V1; k21 <- Qi/V2
  sm <- ke + k12 + k21; dd <- sqrt(sm*sm - 4*ke*k21)
  l1 <- (sm + dd)/2; l2 <- (sm - dd)/2; g <- l1 - l2
  X <- c(0, 0); out <- numeric(nrow(Dv))
  for (i in seq_len(nrow(Dv))[-1]) {
    h <- Dv$TIME[i] - Dv$TIME[i - 1]; R <- Dv$RATE[i - 1]
    e1 <- exp(-l1*h); e2 <- exp(-l2*h); f1 <- 1 - e1; f2 <- 1 - e2
    X <- c((((l1 - k21)*e1 + (k21 - l2)*e2)*X[1] + k21*(e2 - e1)*X[2] +
            R*((l1 - k21)*f1/l1 + (k21 - l2)*f2/l2))/g,
           (k12*(e2 - e1)*X[1] + ((l1 - ke - k12)*e1 + (ke + k12 - l2)*e2)*X[2] +
            R*k12*(f2/l2 - f1/l1))/g)
    out[i] <- X[1]/V1 }
  out }
