library(NonCompart)
# For each subject-dose profile, obtain AUClast, Cmax and the terminal half-life.
pk <- do.call(rbind, lapply(split(dat, ~ Subject + Dose), function(d) {
  r <- sNCA(d$Time, d$conc, dose = d$Dose[1], adm = "Extravascular",
            concUnit = "ug/L", R2ADJ = 0)
  data.frame(Subject = d$Subject[1], Dose = d$Dose[1],
             AUC = r[["AUCLST"]], Cmax = r[["CMAX"]], t.half = r[["LAMZHL"]])
}))
pk$AUC.dn  <- pk$AUC /pk$Dose     # dose-normalized AUC
pk$Cmax.dn <- pk$Cmax/pk$Dose

# If linear, the dose-normalized exposure must be constant regardless of dose.
aggregate(cbind(AUC, Cmax, AUC.dn, Cmax.dn, t.half) ~ Dose, data = pk,
          FUN = mean)
