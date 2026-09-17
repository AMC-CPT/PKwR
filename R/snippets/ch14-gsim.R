# 여러 입원군: 28명이 병상 사정으로 4개 군(10, 8, 6, 4명)으로 나뉘어
# 입원했다. 군간 기저 차이(입원 시기의 계절, 식이 등)는 실재하지만
# 군 x 제형 교호작용은 없는 것이 참인 시나리오다.
set.seed(20260827)
nAdm  <- c(10, 8, 6, 4)
ADMs  <- rep(1:4, nAdm); nG <- sum(nAdm)
seqvG <- unlist(lapply(nAdm, function(k) rep(c("RT", "TR"), each = k/2)))
gd <- data.frame(SUBJ = rep(1:nG, each = 2), ADM = rep(ADMs, each = 2),
                 GRP  = rep(seqvG, each = 2), PRD = rep(1:2, nG))
gd$TRT <- ifelse((gd$GRP == "RT") == (gd$PRD == 1), "R", "T")

admEf <- c(0, 0.08, -0.12, 0.05)         # 군간 기저 차이 (로그척도, 실재)
iivG  <- rnorm(nG, 0, 0.34)
sWg   <- sqrt(log(1 + 0.22^2))           # 개체내 CV 22%
gd$AUClast <- round(exp(log(1800) + admEf[gd$ADM] + iivG[gd$SUBJ] +
                    0.03*(gd$PRD == 2) + log(0.95)*(gd$TRT == "T") +
                    rnorm(2*nG, 0, sWg)), 1)

# 2기 탈락 2명 (군2 에서 1명, 군4 에서 1명): 불완전 개체가 생긴다
gd <- gd[!(gd$SUBJ %in% c(15, 27) & gd$PRD == 2), ]
with(unique(gd[, c("SUBJ", "ADM", "GRP")]), table(ADM, GRP))
c(n.subj = length(unique(gd$SUBJ)), n.rec = nrow(gd))
