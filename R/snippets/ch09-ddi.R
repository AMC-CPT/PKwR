# 응용: CYP 억제제 병용으로 CLint 가 1/5 로 줄 때, AUC 는 몇 배가 되는가
p.i <- p; p.i$CLint <- CLint/5
t2  <- seq(0, 72, 0.01)                     # 반감기가 길어지므로 72시간 적분
iv0 <- as.data.frame(lsoda(yiv, t2, dydt, p))
po0 <- as.data.frame(lsoda(ypo, t2, dydt, p))
iv1 <- as.data.frame(lsoda(yiv, t2, dydt, p.i))
po1 <- as.data.frame(lsoda(ypo, t2, dydt, p.i))

# well-stirred 모형이 주는 종이 위의 예측과 비교한다
ER.i <- fu*p.i$CLint/(QH + fu*p.i$CLint)
CL.i <- QH*ER.i + fu*GFR
round(c(AUCR.iv = auc(t2, iv1$VEN)/auc(t2, iv0$VEN),
        pred.iv = (QH*ER + fu*GFR)/CL.i,
        AUCR.po = auc(t2, po1$VEN)/auc(t2, po0$VEN),
        pred.po = (1 - ER.i)/(1 - ER)*(QH*ER + fu*GFR)/CL.i), 3)
