# 염 인자 S: 염 분자량 가운데 모약물이 차지하는 분율 (화학량론에 주의)
MW <- c(phenytoin  = 252.27, Na.phenytoin    = 274.25,   # 1:1 염
        metoprolol = 267.36, metoprolol.tart = 684.81)   # 2:1 염 (tartrate)
S  <- c(Na.phenytoin    =   MW[["phenytoin"]]/MW[["Na.phenytoin"]],
        metoprolol.tart = 2*MW[["metoprolol"]]/MW[["metoprolol.tart"]])
round(S, 3)

# phenytoin sodium 300 mg 을 경구 투여(F = 0.9 로 가정)할 때의 유효용량 (mg)
round(S[["Na.phenytoin"]]*0.9*300, 1)
