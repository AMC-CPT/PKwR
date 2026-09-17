# 계(생리) 파라미터: 70 kg 성인의 대표값 (L, L/hr; 반올림)
# 간(LI)의 Q 는 간동맥 유입만이며, 문맥 유입은 비장(SP)과 장(GU)을 거친다.
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
CO <- 312                                        # 심박출량 5.2 L/min

# 제약 검증: 혈류 합 = 심박출량, 부피 합 <= 체중
tis    <- c("AD", "BO", "BR", "HT", "KI", "MU", "SK", "SP", "GU", "LI")
ven.in <- c("AD", "BO", "BR", "HT", "KI", "MU", "SK")   # 유출이 정맥혈로
c(Q.total = sum(Q[tis]), CO = CO, V.total = sum(V), QH = sum(Q[c("SP", "GU", "LI")]))

# 대조: ICRP Publication 89 성인 남성 기준값(심박출량 대비 %). 이 모형은 조직을
# 열 개로 줄였으므로 ICRP 가 갑상선/생식선/부신/방광/림프절 등에 준 나머지
# 7.5% 를 대표 조직이 나누어 갖는다. GU 는 위-식도+소장+대장+췌장의 합이다.
icrp <- c(AD = 5.0, BO = 5.0, BR = 12, HT = 4.0, KI = 19,
          MU = 17, SK = 5.0, SP = 3.0, GU = 16, LI = 6.5)
rbind(model = round(100*Q[tis]/CO, 1), ICRP89 = icrp[tis],
      diff = round(100*Q[tis]/CO - icrp[tis], 1))
round(c(sum.model = sum(100*Q[tis]/CO), sum.ICRP = sum(icrp),
        QH.model = sum(Q[c("SP", "GU", "LI")]), QH.ICRP = 0.255*CO), 1)
