# Sample size: the within-subject CV and the expected GMR are the only inputs. The
# author's BE package (sscv: CV in %, returns n 'per sequence') and PowerTOST
# (sampleN.TOST: CV as a fraction, returns the total n) both give it.
library(PowerTOST)
2*sscv(CV = 25, True.R = 0.95)                     # BE package: 28 in total
sampleN.TOST(CV = 0.25, theta0 = 0.95, design = "2x2", targetpower = 0.8,
             print = FALSE)[c("Sample size", "Achieved power")]

# CV x GMR grid: total subjects needed for 80% power (exact method)
cvs <- c(0.15, 0.20, 0.25, 0.30, 0.35)
tab <- sapply(c(0.95, 0.90), function(r) sapply(cvs, function(cv)
         sampleN.TOST(CV = cv, theta0 = r, design = "2x2",
                      print = FALSE)[["Sample size"]]))
dimnames(tab) <- list(paste0("CV ", 100*cvs, "%"), paste("GMR", c(0.95, 0.90)))
tab

# BE needs AUC and Cmax 'both' to pass, so the real power is the joint power of the
# two metrics, and it is exact only if their correlation is used (power.2TOST).
# With n = 28, AUC CV 20%/GMR 0.95, Cmax CV 26%/GMR 1.06 and correlation 0.75:
round(c(AUC.alone  = power.TOST(CV = 0.20, theta0 = 0.95, n = 28),
        Cmax.alone = power.TOST(CV = 0.26, theta0 = 1.06, n = 28),
        joint = power.2TOST(theta0 = c(0.95, 1.06), CV = c(0.20, 0.26),
                            n = 28, rho = 0.75)), 4)
