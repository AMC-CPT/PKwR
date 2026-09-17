# (1) 점검용 전체 모형: 군, 순서군, 군x순서군 내 개체, 군 내 시기, 제형,
#     군x제형. 군x제형 교호작용의 유의성만 여기서 읽는다 (FDA 2026).
gdf <- af(gd, c("SUBJ", "ADM", "GRP", "PRD", "TRT"))
ff  <- log(AUClast) ~ ADM + GRP + ADM:GRP:SUBJ + ADM:PRD + TRT + ADM:TRT
GLM(ff, gdf)$`Type III`

# (2) 주분석은 교호작용과 군을 뺀 표준 모형: 완전한 개체만 쓰는 GLM
fr  <- log(AUClast) ~ GRP/SUBJ + PRD + TRT
ciG <- setNames(exp(CIest(fr, gdf, "TRT", c(-1, 1),
                    conf.level = 0.90)[1, 1:3]), c("PE", "LL", "UL"))
round(ciG, 4)

# (3) 혼합효과 모형(REML): 탈락 개체의 1기 자료까지 전부 쓴다
library(nlme)
lmeG <- lme(log(AUClast) ~ GRP + PRD + TRT, random = ~1 | SUBJ, data = gdf)
round(exp(intervals(lmeG, 0.90)$fixed["TRTT", ]), 4)
c(n.GLM = sum(table(gdf$SUBJ) == 2), n.lme = length(unique(gdf$SUBJ)))
