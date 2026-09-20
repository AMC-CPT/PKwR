# 실측 자료: R 내장 Theoph (theophylline 경구 320 mg 단회, 12명)
# Theoph 의 Dose 열(mg/kg)은 320 mg 에서 파생된 값이라 되곱하면 반올림 오차가
# 따라오고 9번에는 오기까지 있다. 아는 값 320 을 그대로 쓴다.
tb <- tblNCA(Theoph, key = "Subject", colTime = "Time", colConc = "conc",
             dose = 320, adm = "Extravascular", concUnit = "mg/L")
tb[, c("Subject", "CMAX", "TMAX", "LAMZHL", "AUCIFO", "AUCPEO", "CLFO")]

# 요약: 기하평균과 기하 CV% (15장의 노출 요약 관례)
gm  <- function(x) exp(mean(log(x)))
gcv <- function(x) sqrt(exp(var(log(x))) - 1)*100
round(sapply(tb[, c("CMAX", "AUCIFO", "CLFO")], function(x) c(gmean = gm(x),
             gCV = gcv(x))), 2)
