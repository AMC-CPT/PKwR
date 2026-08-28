# 투과제한: 막 투과가 느리면 조직이 혈류를 따라가지 못한다. 뇌를 예로 든다.
Cart <- approxfun(iv$time, iv$ART, rule = 2)      # 전신 모형이 준 동맥혈 농도
perm <- function(PS, org = "BR", fv = 0.03) {     # fv: 조직 중 혈관강 분율
  Vt <- V[[org]]; Vv <- Vt*fv; Vi <- Vt - Vv
  d <- function(t, y, .) { J <- PS*(y[1] - y[2]/Kp[[org]])   # 막 투과 flux
    list(c((Q[[org]]*(Cart(t) - y[1]) - J)/Vv, J/Vi)) }
  o <- as.data.frame(lsoda(c(Cv = 0, Ct = 0), tsim, d, NULL))
  o$Ct }                                          # 혈관강 밖 조직의 농도

PSv <- c(0.5, 5, 50, 500)                         # L/hr (뇌 혈류는 42 L/hr)
Cp  <- sapply(PSv, perm)                          # 4개 PS 에 대해
summ <- function(x) c(Cmax = max(x), tmax = tsim[which.max(x)],
                      AUC = auc(tsim, x))
tab <- cbind(sapply(seq_along(PSv), function(j) summ(Cp[, j])),
            perfusion = summ(iv$BR))
colnames(tab)[seq_along(PSv)] <- paste0("PS=", PSv)
round(rbind(tab, PS.over.Q = c(PSv/Q[["BR"]], Inf)), 3)

matplot(tsim, Cp, type = "l", lty = 2:5, col = 1, las = 1, bty = "l",
        xlim = c(0, 8), ylim = c(0, 4), xlab = "Time (hr)",
        ylab = "Brain concentration (mg/L)")
lines(iv$time, iv$BR, lwd = 1.8)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2:5), lwd = c(1.8, rep(1, 4)),
       legend = c("관류제한", paste("PS =", PSv)))
