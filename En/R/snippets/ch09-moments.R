# IV bolus has no C0: back-extrapolate the log line through the first two points
C0 <- exp(log(dat2$DV[1]) -
          dat2$Time[1]*diff(log(dat2$DV[1:2]))/diff(dat2$Time[1:2]))
a0 <- auc.ld(c(0, dat2$Time[1]), c(C0, dat2$DV[1]))    # area from 0 to t1

# Extrapolation: AUCinf = AUClast + Clast/lambda.z, and the extrapolated fraction
Cl <- dat2$DV[nrow(dat2)]; tl <- dat2$Time[nrow(dat2)]; lam <- lz[["lambda.z"]]
AUC.inf <- a0 + auc.ld(dat2$Time, dat2$DV) + Cl/lam
pct.ext <- (Cl/lam)/AUC.inf*100

# Area under the first moment curve (t*C): the extrapolation has two terms
AUMC.inf <- auc.lin(c(0, dat2$Time), c(0, dat2$Time*dat2$DV)) +
            Cl*tl/lam + Cl/lam^2

MRT <- AUMC.inf/AUC.inf
round(c(C0 = C0, AUC.inf = AUC.inf, pct.extrap = pct.ext,
        AUMC.inf = AUMC.inf, MRT = MRT, inv.MRT = 1/MRT), 4)

# Clearance and volumes from areas alone (true: CL 4.5, Vss 60, Vz 69.9)
round(c(CL = D/AUC.inf, Vss = D/AUC.inf*MRT, Vz = D/AUC.inf/lam), 3)
