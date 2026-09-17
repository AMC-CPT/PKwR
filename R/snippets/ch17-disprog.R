# 선형 질병진행 위에 겹친 두 가지 약효. 점수가 클수록 나쁜 척도이며,
# 84일 치료 뒤 휴약(washout)에서의 갈림이 두 작용을 구별해 준다.
S0 <- 30; alpha <- 0.1; Toff <- 84                # 자연 경과: 하루 0.1 점 악화
t   <- 0:180
nat <- S0 + alpha*t                               # 자연 경과
sym <- nat - 6*(t <= Toff)                        # 증상완화: 중단 즉시 이득 소실
mod <- S0 + ifelse(t <= Toff, (alpha - 0.06)*t,   # 질병수정: 기울기를 줄인다
                   (alpha - 0.06)*Toff + alpha*(t - Toff))

plot(t, nat, type = "l", las = 1, bty = "l", ylim = c(20, 50),
     xlab = "Time (day)", ylab = "Disease status (score)")
lines(t, sym, lty = 2); lines(t, mod, lty = 4)
abline(v = Toff, lty = 3); text(Toff, 21, "stop", pos = 4, cex = 0.8)
legend("topleft", bty = "n", cex = 0.8, lty = c(1, 2, 4),
       legend = c("natural", "symptomatic", "disease-modifying"))

# 치료 중(84일)에는 비슷해 보이던 두 약이 휴약 뒤(180일)에는 갈라진다
res <- rbind(natural = nat[c(85, 181)], symptomatic = sym[c(85, 181)],
             modifying = mod[c(85, 181)])
colnames(res) <- c("day 84", "day 180"); res
