# 정맥 일시주입은 C0 이 없다: 처음 두 점의 로그 직선으로 역외삽해 채운다
C0 <- exp(log(dat2$DV[1]) -
          dat2$Time[1]*diff(log(dat2$DV[1:2]))/diff(dat2$Time[1:2]))
a0 <- auc.ld(c(0, dat2$Time[1]), c(C0, dat2$DV[1]))    # 0 ~ t1 면적

# 외삽: AUCinf = AUClast + Clast/lambda.z, 그리고 외삽 비율
Cl <- dat2$DV[nrow(dat2)]; tl <- dat2$Time[nrow(dat2)]; lam <- lz[["lambda.z"]]
AUC.inf <- a0 + auc.ld(dat2$Time, dat2$DV) + Cl/lam
pct.ext <- (Cl/lam)/AUC.inf*100

# 일차 적률 곡선(t*C)의 면적: 외삽 항이 두 개다
AUMC.inf <- auc.lin(c(0, dat2$Time), c(0, dat2$Time*dat2$DV)) +
            Cl*tl/lam + Cl/lam^2

MRT <- AUMC.inf/AUC.inf
round(c(C0 = C0, AUC.inf = AUC.inf, pct.extrap = pct.ext,
        AUMC.inf = AUMC.inf, MRT = MRT, inv.MRT = 1/MRT), 4)

# 면적만으로 얻는 청소율과 분포용적 (참값: CL 4.5, Vss 60, Vz 69.9)
round(c(CL = D/AUC.inf, Vss = D/AUC.inf*MRT, Vz = D/AUC.inf/lam), 3)
