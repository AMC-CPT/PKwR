# Dose adjustment in renal impairment: the lever is fe (fraction excreted unchanged)
# Assumption: renal CL is proportional to CLcr, nonrenal CL unchanged (Giusti-Hayton)
adj <- function(fe, KF) (1 - fe) + fe*KF    # dose (clearance) adjustment factor
KF  <- 30/120                               # patient CLcr 30, normal 120 mL/min
round(c(fe.0.9 = adj(0.9, KF), fe.0.3 = adj(0.3, KF)), 3)
