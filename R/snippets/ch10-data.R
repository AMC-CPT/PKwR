# Theoph 를 NONMEM 형식으로: ID, TIME, DV 와 용량(DOSE, mg), 공변량(BWT)
DATA <- data.frame(ID   = as.numeric(as.character(Theoph$Subject)),
                   TIME = Theoph$Time, DV = Theoph$conc,
                   BWT  = Theoph$Wt,   DOSE = round(Theoph$Dose*Theoph$Wt, 1))
DATA <- DATA[order(DATA$ID, DATA$TIME), ]     # ID 오름차순 정렬 [필수]
stopifnot(!is.unsorted(DATA$ID))              # 정렬 assertion (저장 직전 검증)
head(DATA, 3)
c(n.subj = length(unique(DATA$ID)), n.rec = nrow(DATA),
  dose.min = min(DATA$DOSE), dose.max = max(DATA$DOSE))
