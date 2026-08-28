# 정렬: sort 는 값을, order 는 '순서'를 돌려준다. 표를 정렬할 때는 언제나 order 다.
d2 <- data.frame(ID = c(3, 1, 2, 1, 3, 2), TIME = c(2, 2, 0, 0, 0, 2))
d2$DV <- round(decay(d2$TIME, A = 70 + 10*d2$ID), 2)
d2 <- d2[order(d2$ID, d2$TIME), ]             # ID 오름차순, 그 안에서 TIME 순
row.names(d2) <- NULL; d2

# 개체별 공변량 붙이기: 행 순서와 행 수를 지켜야 하므로 merge 대신 match
cov1 <- data.frame(ID = c(2, 1, 3), WT = c(75, 62, 88))
d2$WT <- cov1$WT[match(d2$ID, cov1$ID)]       # 왼쪽 표의 순서가 그대로 보존된다
stopifnot(nrow(d2) == 6, !anyNA(d2$WT))
head(d2, 3)

# 긴 꼴 -> 넓은 꼴 (13장 교차설계의 T/R 짝짓기가 이 모양이다)
reshape(d2[, c("ID", "TIME", "DV")], idvar = "ID", timevar = "TIME",
        direction = "wide")
