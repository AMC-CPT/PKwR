# Dedrick plot: a change of coordinates superimposes the curves of different species.
# If CL = a W^0.75 and V = c W^1, k = CL/V is proportional to W^-0.25, so with
# time as t/W^0.25 and concentration as C/(D/W), all species lie on one curve.
Wd  <- c(mouse = 0.02, rat = 0.25, monkey = 5, dog = 12)
aCL <- 0.9; bCL <- 0.75; cV <- 5                  # CL (L/hr), V (L)
kW  <- function(W) aCL*W^bCL/(cV*W)               # elimination rate constant
Cd  <- function(t, W) 10*W/(cV*W)*exp(-kW(W)*t)   # 10 mg/kg IV bolus
round(rbind(W = Wd, k = kW(Wd), t.half = log(2)/kW(Wd)), 4)

par(mfrow = c(1, 2), mar = c(4.2, 6.4, 2.4, 0.8))
tg <- seq(0.01, 40, 0.05); ii <- seq(1, length(tg), 40)
matplot(tg, sapply(Wd, function(W) Cd(tg, W)), type = "l", log = "y", lty = 1:4,
        col = 1, las = 1, bty = "l", ylim = c(0.005, 3), xlab = "t (hr)",
        ylab = "", main = "(a) original coordinates")
title(ylab = "C (mg/L)", line = 5)      # axis title outside the tick labels
legend("topright", bty = "n", cex = 0.75, lty = 1:4, legend = names(Wd))
matplot(sapply(Wd, function(W) tg[ii]/W^(1 - bCL)),
        sapply(Wd, function(W) Cd(tg[ii], W)/10), type = "p", pch = 1:4,
        cex = 0.7, col = 1, log = "y", las = 1, bty = "l", xlim = c(0, 12),
        ylim = c(0.005, 0.3), xlab = expression(t/W^0.25),
        ylab = "", main = "(b) Dedrick coordinates")
title(ylab = "C/(D/W)", line = 5)
legend("topright", bty = "n", cex = 0.75, pch = 1:4, legend = names(Wd))

# The common curve mapped back to human (70 kg) coordinates is the human prediction
Wh <- 70; th <- c(1, 6, 12, 24)
teq <- th/Wh^(1 - bCL)                            # species-independent time
round(rbind(t.human = th, t.equivalent = teq,
            from.mouse = Cd(teq*Wd[["mouse"]]^(1 - bCL), Wd[["mouse"]]),
            exact.human = Cd(th, Wh)), 4)
