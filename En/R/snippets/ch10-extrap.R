# Special populations: keep the drug parameters, swap in the system parameters
scen <- function(f.CLint = 1, f.QH = 1, f.fu = 1, f.GFR = 1) {
  pp <- p
  pp$CLint <- CLint*f.CLint; pp$fu <- fu*f.fu; pp$GFR <- GFR*f.GFR
  pp$Q[c("LI", "SP", "GU")] <- Q[c("LI", "SP", "GU")]*f.QH
  pp$CO <- sum(pp$Q[tis])                         # rebalance the blood flows
  pp }
grp <- list(healthy   = scen(),                            # healthy adult
            cirrhosis = scen(f.CLint = .4, f.QH = .7, f.fu = 1.4),  # cirrhosis
            renal     = scen(f.GFR = .2))                  # severe renal impairment

out <- t(sapply(grp, function(pp) {
  a <- as.data.frame(lsoda(yiv, tsen, dydt, pp))
  b <- as.data.frame(lsoda(ypo, tsen, dydt, pp))
  QHi <- sum(pp$Q[c("SP", "GU", "LI")])
  ERi <- pp$fu*pp$CLint/(QHi + pp$fu*pp$CLint)             # well-stirred
  c(ER = ERi, F.pred = Fa*(1 - ERi), CL.pred = QHi*ERi + pp$fu*pp$GFR,
    AUC.iv = auc(a$time, a$VEN), AUC.po = auc(b$time, b$VEN)) }))
round(cbind(out[, 1:3],
            R.iv = out[, "AUC.iv"]/out[1, "AUC.iv"],
            R.iv.pred = out[1, "CL.pred"]/out[, "CL.pred"],
            R.po = out[, "AUC.po"]/out[1, "AUC.po"],
            R.po.pred = out[, "F.pred"]/out[1, "F.pred"]*
                        out[1, "CL.pred"]/out[, "CL.pred"]), 3)
