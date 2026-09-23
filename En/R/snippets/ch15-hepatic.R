# Well-stirred liver model: what changes, and how, when CLint, QH or fub doubles
# Assumed: hepatic elimination only, full absorption (liver-only first pass), fixed V
hep <- function(CLint, QH, fub, D = 100, V = 50) {
  ER  <- fub*CLint/(QH + fub*CLint)
  CLH <- QH*ER
  data.frame(ER = ER, CLH = CLH, F = 1 - ER, t.half = log(2)*V/CLH,
             AUC.iv = D/CLH, AUC.po = (1 - ER)*D/CLH)
}
scen <- function(CLint, QH, fub)
  rbind(baseline = hep(CLint, QH, fub),
        CLint.x2 = hep(2*CLint, QH, fub),
        QH.x2    = hep(CLint, 2*QH, fub),
        fub.x2   = hep(CLint, QH, 2*fub))

list(low.ER  = round(scen(  30, 90, 0.1), 3),   # fub*CLint =   3 << QH
     high.ER = round(scen(9000, 90, 0.1), 3))   # fub*CLint = 900 >> QH
