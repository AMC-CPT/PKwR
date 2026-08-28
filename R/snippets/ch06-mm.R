# 내적 청소율은 Michaelis-Menten 식에서 나온다: CLint = Vmax/(Km + C)
CLint <- function(C, Vmax, Km) Vmax/(Km + C)

# well-stirred model 의 추출률과 간 청소율
ER <- function(CLint, QH, fub = 1) fub*CLint/(QH + fub*CLint)
CLH <- function(CLint, QH, fub = 1) QH*ER(CLint, QH, fub)

QH <- 90                                        # 간혈류량 L/hr
data.frame(CLint = c(5, 30, 300, 3000),
           ER  = round(ER (c(5, 30, 300, 3000), QH), 3),
           CLH = round(CLH(c(5, 30, 300, 3000), QH), 1),
           type = c("low ER (capacity limited)", "intermediate",
                    "high ER (flow limited)", "high ER (flow limited)"))
