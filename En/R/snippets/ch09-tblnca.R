# Real data: R's built-in Theoph (oral theophylline 320 mg single dose, 12 subjects)
# The Dose column of Theoph (mg/kg) is derived from 320 mg, so multiplying back
# brings rounding error, and subject 9 even has a typo. Use the known value 320.
tb <- tblNCA(Theoph, key = "Subject", colTime = "Time", colConc = "conc",
             dose = 320, adm = "Extravascular", concUnit = "mg/L")
tb[, c("Subject", "CMAX", "TMAX", "LAMZHL", "AUCIFO", "AUCPEO", "CLFO")]

# Summary: geometric mean and geometric CV% (exposure summary convention of chapter 15)
gm  <- function(x) exp(mean(log(x)))
gcv <- function(x) sqrt(exp(var(log(x))) - 1)*100
round(sapply(tb[, c("CMAX", "AUCIFO", "CLFO")], function(x) c(gmean = gm(x),
             gCV = gcv(x))), 2)
