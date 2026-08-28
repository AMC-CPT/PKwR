# 신청소율의 판독: 실측 CLr 를 여과 성분 fu*GFR 에 견준다 (mL/min)
GFR <- 125
ren <- data.frame(row.names = c("inulin", "PAH", "glucose", "digoxin",
                                "penicillin G"),
                  fu  = c(1, 0.9, 1, 0.75, 0.45),
                  CLr = c(125, 600, 0, 110, 500))
ren$fuGFR <- ren$fu*GFR
ren$ratio <- round(ren$CLr/ren$fuGFR, 2)
ren$net   <- cut(ren$ratio, c(-Inf, 0.8, 1.2, Inf),
                 c("reabsorption", "filtration", "secretion"))
ren
