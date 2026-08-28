# 민감도 분석: 파라미터를 10% 올렸을 때 결과가 몇 % 움직이는가(정규화 민감도)
tsen <- c(seq(0, 24, 0.02), seq(24.5, 120, 0.5))   # 꼬리까지 적분해야 AUC 가 맞다
metrics <- function(pp) {
  a <- as.data.frame(lsoda(yiv, tsen, dydt, pp))
  b <- as.data.frame(lsoda(ypo, tsen, dydt, pp))
  c(AUC.iv = auc(a$time, a$VEN), AUC.po = auc(b$time, b$VEN),
    Cmax.po = max(b$VEN)) }
bump <- function(nm, f = 1.1) {
  pp <- p
  if (grepl("^Kp[.]", nm)) { k <- sub("^Kp[.]", "", nm); pp$Kp[k] <- pp$Kp[k]*f }
  else if (grepl("^Q[.]", nm)) { k <- sub("^Q[.]", "", nm)
    pp$Q[k] <- pp$Q[k]*f; pp$CO <- sum(pp$Q[tis]) }        # 혈류 수지를 다시 맞춘다
  else if (nm == "CO") { pp$Q <- pp$Q*f; pp$CO <- pp$CO*f }
  else pp[[nm]] <- pp[[nm]]*f
  pp }

base <- metrics(p)
pars <- c("CLint", "fu", "GFR", "ka", "Kp.AD", "Kp.MU", "Q.LI", "CO")
S <- t(sapply(pars, function(nm) (metrics(bump(nm))/base - 1)/0.1))
S <- S[order(-abs(S[, "AUC.po"])), ]
round(S, 3)

par(mar = c(4.2, 6.0, 1.0, 1.0))                  # 토네이도 그림
barplot(rev(S[, "AUC.po"]), horiz = TRUE, las = 1, cex.names = 0.85,
        xlab = "정규화 민감도 (경구 AUC)", col = "gray85", border = "gray40")
abline(v = 0, lwd = 1.1)
