library(NonCompart)
nca <- sNCA(dat2$Time, dat2$DV, dose = D, adm = "Bolus", down = "Log",
            doseUnit = "mg", concUnit = "mg/L")
round(nca[c("LAMZNPT", "LAMZ", "LAMZHL", "CLSTP", "AUCLST", "AUCIFO",
            "AUCIFP", "AUCPEO", "AUMCIFO", "MRTIVIFO", "CLO", "CLP",
            "VZO", "VZP", "VSSO")], 4)
