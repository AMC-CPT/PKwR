# Theoph 를 NONMEM 형식으로: ID, TIME, DV 와 용량(DOSE, mg), 공변량(BWT)
# 이 시험은 12명 모두 320 mg 단일 경구 투여다. Theoph 의 Dose 열(mg/kg)은
# 거기서 파생된 값이라 반올림 오차를 싣고 9번에는 오기가 있으므로 쓰지 않는다.
DATA <- data.frame(ID   = as.numeric(as.character(Theoph$Subject)),
                   TIME = Theoph$Time, DV = Theoph$conc,
                   BWT  = Theoph$Wt,   DOSE = 320)
DATA <- DATA[order(DATA$ID, DATA$TIME), ]     # ID 오름차순 정렬 [필수]
stopifnot(!is.unsorted(DATA$ID))              # 정렬 assertion (저장 직전 검증)
head(DATA, 3)
c(n.subj = length(unique(DATA$ID)), n.rec = nrow(DATA),
  dose.min = min(DATA$DOSE), dose.max = max(DATA$DOSE))
