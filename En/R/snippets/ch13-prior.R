# Prior: the FOCE-I final estimates from the Theoph data in Chapter 11, used as is.
# Theophylline oral 1-cmt: KA, V, K (apparent oral parameters; F absorbed into V, CL)
TH <- c(KA = 1.4950, V = 32.4770, K = 0.0872)
OM <- matrix(c( 0.4363, 0.0573, -0.0067,            # eta covariance (full block)
                0.0573, 0.0199,  0.0117,
               -0.0067, 0.0117,  0.0206), 3, 3)
SG <- c(prop = 0.0175, add = 0.0787)                # residual variances (prop, add)

ipred <- function(eta, t, D) {                      # individual prediction (Bateman)
  ka <- TH[[1]]*exp(eta[1]); v <- TH[[2]]*exp(eta[2]); k <- TH[[3]]*exp(eta[3])
  D/v*ka/(ka - k)*(exp(-k*t) - exp(-ka*t))
}
round(c(CL.pop = TH[[2]]*TH[[3]], t.half.pop = log(2)/TH[[3]],
        IIV.CV = 100*sqrt(exp(diag(OM)) - 1)), 2)
