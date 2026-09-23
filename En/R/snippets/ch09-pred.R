# Predicted Clast: the value of the terminal regression line at tlast. Uses the
# information of all points in the regression instead of one noisy observed Clast.
b0 <- coef(lm(log(DV) ~ Time, dat2[used, ]))[[1]]     # intercept of terminal regression
Cl.pred <- exp(b0 - lam*tl)

AUC.lst <- a0 + auc.ld(dat2$Time, dat2$DV)            # = AUClast (incl. C0 interval)
AUC.ifp <- AUC.lst + Cl.pred/lam
round(c(Clast.obs = Cl, Clast.pred = Cl.pred), 4)
round(c(AUCIFO = AUC.inf,     AUCIFP = AUC.ifp,
        CLO    = D/AUC.inf,   CLP    = D/AUC.ifp,
        VZO    = D/AUC.inf/lam, VZP  = D/AUC.ifp/lam), 4)
