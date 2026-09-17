# 검산 1: 질량수지 - 체내 잔존량 + 제거 누적량 = 투여량
last <- iv[nrow(iv), ]
organs <- c("LU", tis, "ART", "VEN")
remain <- sum(unlist(last[organs])*V[organs])
round(c(remain = remain, eliminated = last$AEL,
        recovery = (remain + last$AEL + last$AGL)/D), 4)

# 검산 2: 정맥혈장 곡선의 NCA 가 종이 위의 예측을 되돌려 주는가
library(NonCompart)
nca <- sNCA(iv$time, iv$VEN, dose = D, adm = "Bolus", down = "Log",
            doseUnit = "mg", concUnit = "mg/L")
round(nca[c("CLO", "VSSO", "MRTIVIFO", "LAMZHL", "AUCIFO")], 3)
