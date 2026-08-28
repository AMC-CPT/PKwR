# 신기능 저하 시 용량 조절: 지렛대는 fe (미변화체 배설 분율)
# 가정: 신청소율은 CLcr 에 비례하고 비신성 청소율은 불변 (Giusti-Hayton)
adj <- function(fe, KF) (1 - fe) + fe*KF    # 용량(청소율) 조절 계수
KF  <- 30/120                               # 환자 CLcr 30, 정상 120 mL/min
round(c(fe.0.9 = adj(0.9, KF), fe.0.3 = adj(0.3, KF)), 3)
