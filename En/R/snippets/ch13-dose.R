# Dose adjustment: target mean steady-state concentration Cav = 10 mg/L, interval 12 h.
# Maintenance dose = Cav x CL x tau. With an oral prior, CL is CL/F, and
# the resulting dose is the 'oral dose' as it stands.
CL.map <- TH[[2]]*exp(fit5[2])*TH[[3]]*exp(fit5[3])
D.pop <- 10*TH[[2]]*TH[[3]]*12                      # dose from population prior only
D.new <- 10*CL.map*12                               # adjusted by individual MAP CL
round(c(CL.pop = TH[[2]]*TH[[3]], CL.map = CL.map,
        D.pop = D.pop, D.new = D.new), 1)

# Steady-state prediction (analytical solution for repeated oral 1-cmt dosing)
css <- function(eta, t, D, tau) {
  ka <- TH[[1]]*exp(eta[1]); v <- TH[[2]]*exp(eta[2]); k <- TH[[3]]*exp(eta[3])
  tt <- t %% tau
  D/v*ka/(ka - k)*(exp(-k*tt)/(1 - exp(-k*tau)) -
                   exp(-ka*tt)/(1 - exp(-ka*tau)))
}
tg <- seq(0, 24, 0.05)
plot(tg, css(eta.true, tg, D.pop, 12), type = "l", lty = 2, las = 1,
     bty = "l", ylim = c(0, 16), xlab = "Time after dose (hr)",
     ylab = "Css (mg/L)", panel.first = {
       polygon(c(0, 24, 24, 0), c(5, 5, 15, 15), col = "gray92", border = NA)
       abline(h = 10, lty = 3) })
lines(tg, css(eta.true, tg, D.new, 12))
legend("topright", bty = "n", cex = 0.85, lty = c(2, 1),
       legend = c(sprintf("population dose %.0f mg", D.pop),
                  sprintf("MAP-adjusted %.0f mg", D.new)))
text(23.3, 5.9, "5-15 mg/L", cex = 0.75, adj = 1)

# Actual (true) average concentration after adjustment: how close to the target of 10?
round(c(Cav.pop.dose = D.pop/(12*TH[[2]]*exp(eta.true[2])*TH[[3]]*exp(eta.true[3])),
        Cav.new.dose = D.new/(12*TH[[2]]*exp(eta.true[2])*TH[[3]]*exp(eta.true[3]))), 2)
