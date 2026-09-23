# Application: a CYP inhibitor cuts CLint to 1/5; by what factor does AUC change?
p.i <- p; p.i$CLint <- CLint/5
t2  <- seq(0, 72, 0.01)                     # half-life lengthens: integrate to 72 hr
iv0 <- as.data.frame(lsoda(yiv, t2, dydt, p))
po0 <- as.data.frame(lsoda(ypo, t2, dydt, p))
iv1 <- as.data.frame(lsoda(yiv, t2, dydt, p.i))
po1 <- as.data.frame(lsoda(ypo, t2, dydt, p.i))

# compare with the prediction on paper from the well-stirred model
ER.i <- fu*p.i$CLint/(QH + fu*p.i$CLint)
CL.i <- QH*ER.i + fu*GFR
round(c(AUCR.iv = auc(t2, iv1$VEN)/auc(t2, iv0$VEN),
        pred.iv = (QH*ER + fu*GFR)/CL.i,
        AUCR.po = auc(t2, po1$VEN)/auc(t2, po0$VEN),
        pred.po = (1 - ER.i)/(1 - ER)*(QH*ER + fu*GFR)/CL.i), 3)
