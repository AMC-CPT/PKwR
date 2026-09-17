# 예측 Clast: 말기 회귀직선이 tlast 에서 주는 값. 관측 Clast 한 점의
# 잡음 대신 회귀에 쓴 점 전체의 정보를 쓴다.
b0 <- coef(lm(log(DV) ~ Time, dat2[used, ]))[[1]]     # 말기 회귀의 절편
Cl.pred <- exp(b0 - lam*tl)

AUC.lst <- a0 + auc.ld(dat2$Time, dat2$DV)            # = AUClast (C0 구간 포함)
AUC.ifp <- AUC.lst + Cl.pred/lam
round(c(Clast.obs = Cl, Clast.pred = Cl.pred), 4)
round(c(AUCIFO = AUC.inf,     AUCIFP = AUC.ifp,
        CLO    = D/AUC.inf,   CLP    = D/AUC.ifp,
        VZO    = D/AUC.inf/lam, VZP  = D/AUC.ifp/lam), 4)
