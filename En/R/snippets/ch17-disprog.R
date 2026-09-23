# Two drug effects superimposed on linear disease progression (higher score is worse);
# the divergence at washout after 84 days of treatment separates the two actions.
S0 <- 30; alpha <- 0.1; Toff <- 84                # natural course: 0.1 point/day worse
t   <- 0:180
nat <- S0 + alpha*t                               # natural course
sym <- nat - 6*(t <= Toff)                        # symptomatic: gain lost on stopping
mod <- S0 + ifelse(t <= Toff, (alpha - 0.06)*t,   # disease-modifying: lowers the slope
                   (alpha - 0.06)*Toff + alpha*(t - Toff))

plot(t, nat, type = "l", las = 1, bty = "l", ylim = c(20, 50),
     xlab = "Time (day)", ylab = "Disease status (score)")
lines(t, sym, lty = 2); lines(t, mod, lty = 4)
abline(v = Toff, lty = 3); text(Toff, 21, "stop", pos = 4, cex = 0.8)
legend("topleft", bty = "n", cex = 0.8, lty = c(1, 2, 4),
       legend = c("natural", "symptomatic", "disease-modifying"))

# The two drugs, similar during treatment (day 84), diverge after washout (day 180)
res <- rbind(natural = nat[c(85, 181)], symptomatic = sym[c(85, 181)],
             modifying = mod[c(85, 181)])
colnames(res) <- c("day 84", "day 180"); res
