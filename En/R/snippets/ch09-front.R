# NonCompart's C0 rule (intravenous bolus only): if the first two positive
# concentrations decline (C1 > C2), log back-extrapolate; otherwise use the first
c0.rule <- function(t, C) {
  i <- which(C > 0)[1:2]
  if (C[i[1]] > C[i[2]])
    exp(log(C[i[1]]) - t[i[1]]*diff(log(C[i]))/diff(t[i]))
  else C[i[1]]
}
round(c(C0 = c0.rule(dat2$Time, dat2$DV), AUC.backext = a0,
        pct.backext = a0/AUC.inf*100), 4)
