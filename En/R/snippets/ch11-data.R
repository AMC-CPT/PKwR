# Theoph in NONMEM format: ID, TIME, DV, the dose (DOSE, mg) and a covariate (BWT).
# All 12 subjects received a single oral dose of 320 mg. The Dose column of Theoph
# (mg/kg) is derived from it, with rounding error and a typo in subject 9, so not used.
DATA <- data.frame(ID   = as.numeric(as.character(Theoph$Subject)),
                   TIME = Theoph$Time, DV = Theoph$conc,
                   BWT  = Theoph$Wt,   DOSE = 320)
DATA <- DATA[order(DATA$ID, DATA$TIME), ]     # sort by ascending ID [mandatory]
stopifnot(!is.unsorted(DATA$ID))              # sort assertion (verify before saving)
head(DATA, 3)
c(n.subj = length(unique(DATA$ID)), n.rec = nrow(DATA),
  dose.min = min(DATA$DOSE), dose.max = max(DATA$DOSE))
