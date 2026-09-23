# Check 1: mass balance - amount remaining in body + cumulative eliminated = dose
last <- iv[nrow(iv), ]
organs <- c("LU", tis, "ART", "VEN")
remain <- sum(unlist(last[organs])*V[organs])
round(c(remain = remain, eliminated = last$AEL,
        recovery = (remain + last$AEL + last$AGL)/D), 4)

# Check 2: does NCA of the venous plasma curve return the predictions made on paper?
library(NonCompart)
nca <- sNCA(iv$time, iv$VEN, dose = D, adm = "Bolus", down = "Log",
            doseUnit = "mg", concUnit = "mg/L")
round(nca[c("CLO", "VSSO", "MRTIVIFO", "LAMZHL", "AUCIFO")], 3)
