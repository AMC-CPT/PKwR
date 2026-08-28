# 수치적분: 사다리꼴은 이웃한 두 점을 직선으로 이어 그 아래 면적을 더한다
g  <- function(t) A*exp(-k*t)
trap <- function(t) { y <- g(t); sum(diff(t)*(head(y, -1) + tail(y, -1))/2) }
exactI <- A/k*(1 - exp(-k*20))                # 해석해
round(c(exact = exactI,
        n5  = trap(seq(0, 20, length.out = 5)),
        n21 = trap(seq(0, 20, length.out = 21)),
        n81 = trap(seq(0, 20, length.out = 81)),
        integrate = integrate(g, 0, 20)$value), 4)   # R 의 적응 구적법
