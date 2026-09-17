# 내성(길항 조절자 모형): 약물농도가 가상의 조절자 Cm 의 생성을 구동하고,
# Cm 이 커질수록 효과가 깎인다.  dCm/dt = km0 (Cp - Cm) 은 효과구획과 같은
# 꼴이므로, 일차 흡수 입력의 지수항마다 해석해를 겹쳐 쓴다 (선형계의 중첩).
D <- 150; V <- 15; ka <- 1.2; k <- 0.35; tau <- 12  # 같은 용량 2회, 12시간 간격
km0 <- 0.06; Cm50 <- 2; E0 <- 60; Sl <- 8           # 조절자 제거 t1/2 11.6 hr
C1  <- D/V*ka/(ka - k)
cp1 <- function(t) ifelse(t < 0, 0, C1*(exp(-k*pmax(t, 0)) - exp(-ka*pmax(t, 0))))
g   <- function(t, a) ifelse(t < 0, 0,
         km0/(km0 - a)*(exp(-a*pmax(t, 0)) - exp(-km0*pmax(t, 0))))
cm1 <- function(t) C1*(g(t, k) - g(t, ka))

t  <- seq(0, 36, 0.02)
Cp <- cp1(t) + cp1(t - tau)
Cm <- cm1(t) + cm1(t - tau)
E     <- E0 + Sl*Cp/(1 + Cm/Cm50)                   # 내성이 든 실제 효과
E.ref <- E0 + Sl*Cp                                 # 조절자가 없다면 보일 효과

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(t, E.ref, type = "l", lty = 3, las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Effect", main = "(a) time course")
lines(t, E)
legend("topright", bty = "n", cex = 0.8, lty = c(3, 1),
       legend = c("no tolerance", "with mediator"))

i1 <- t <= tau                                      # 첫 용량 구간의 고리
plot(Cp[i1], E[i1], type = "l", las = 1, bty = "l",
     xlab = "Concentration", ylab = "Effect", main = "(b) proteresis (dose 1)")
j <- c(30, 500)                                     # 상승기(t=0.6)와 하강기(t=10)
arrows(Cp[j], E[j], Cp[j + 15], E[j + 15], length = 0.07)

# 축적 때문에 두 번째 Cmax 가 오히려 높은데도 효과의 두 번째 정점은 낮다
i2 <- t >= tau
round(c(Cp.max1 = max(Cp[i1]), Cp.max2 = max(Cp[i2]),
        E.max1 = max(E[i1]), E.max2 = max(E[i2])), 1)
