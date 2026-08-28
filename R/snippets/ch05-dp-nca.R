library(NonCompart)
# 각 대상자-용량 프로파일에서 AUClast, Cmax, 최종반감기를 구한다.
pk <- do.call(rbind, lapply(split(dat, ~ Subject + Dose), function(d) {
  r <- sNCA(d$Time, d$conc, dose = d$Dose[1], adm = "Extravascular",
            concUnit = "ug/L", R2ADJ = 0)
  data.frame(Subject = d$Subject[1], Dose = d$Dose[1],
             AUC = r[["AUCLST"]], Cmax = r[["CMAX"]], t.half = r[["LAMZHL"]])
}))
pk$AUC.dn  <- pk$AUC /pk$Dose     # 용량 정규화 AUC
pk$Cmax.dn <- pk$Cmax/pk$Dose

# 선형이면 용량 정규화 노출이 용량에 무관하게 일정해야 한다.
aggregate(cbind(AUC, Cmax, AUC.dn, Cmax.dn, t.half) ~ Dose, data = pk,
          FUN = mean)
