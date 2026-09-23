# System (physiological) parameters: typical 70 kg adult values (L, L/hr; rounded)
# The Q of the liver (LI) is the hepatic arterial inflow only; the portal inflow
# passes through the spleen (SP) and the gut (GU).
phys <- read.table(header = TRUE, text = "
org    V     Q
LU    0.5   NA
AD   14.0   18
BO    8.0   15
BR    1.4   42
HT    0.33  14
KI    0.30  70
MU   28.0   45
SK    3.4   18
SP    0.19   5
GU    1.7   60
LI    1.8   25
ART   1.7   NA
VEN   3.4   NA")
rownames(phys) <- phys$org
V <- setNames(phys$V, phys$org); Q <- setNames(phys$Q, phys$org)
CO <- 312                                        # cardiac output 5.2 L/min

# Constraint check: sum of blood flows = cardiac output, sum of volumes <= body weight
tis    <- c("AD", "BO", "BR", "HT", "KI", "MU", "SK", "SP", "GU", "LI")
ven.in <- c("AD", "BO", "BR", "HT", "KI", "MU", "SK")   # outflow goes to venous blood
c(Q.total = sum(Q[tis]), CO = CO, V.total = sum(V), QH = sum(Q[c("SP", "GU", "LI")]))

# Comparison: ICRP Publication 89 adult male reference values (% of cardiac output).
# This model has only ten tissues, so the remaining 7.5% that ICRP allots to thyroid,
# gonads, adrenals, bladder, lymph nodes, etc. is shared by the representative tissues.
# GU is the sum of stomach-esophagus + small intestine + large intestine + pancreas.
icrp <- c(AD = 5.0, BO = 5.0, BR = 12, HT = 4.0, KI = 19,
          MU = 17, SK = 5.0, SP = 3.0, GU = 16, LI = 6.5)
rbind(model = round(100*Q[tis]/CO, 1), ICRP89 = icrp[tis],
      diff = round(100*Q[tis]/CO - icrp[tis], 1))
round(c(sum.model = sum(100*Q[tis]/CO), sum.ICRP = sum(icrp),
        QH.model = sum(Q[c("SP", "GU", "LI")]), QH.ICRP = 0.255*CO), 1)
