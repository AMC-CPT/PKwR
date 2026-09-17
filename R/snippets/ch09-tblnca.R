# 실측 자료: R 내장 Theoph (theophylline 경구, 12명)
dw <- unique(Theoph[, c("Subject", "Wt", "Dose")])       # Dose 는 mg/kg
tb <- tblNCA(Theoph, key = "Subject", colTime = "Time", colConc = "conc",
             dose = dw$Dose*dw$Wt, adm = "Extravascular", concUnit = "mg/L")
tb[, c("Subject", "CMAX", "TMAX", "LAMZHL", "AUCIFO", "AUCPEO", "CLFO")]

# 요약: 기하평균과 기하 CV% (15장의 노출 요약 관례)
gm  <- function(x) exp(mean(log(x)))
gcv <- function(x) sqrt(exp(var(log(x))) - 1)*100
round(sapply(tb[, c("CMAX", "AUCIFO", "CLFO")], function(x) c(gmean = gm(x),
             gCV = gcv(x))), 2)
