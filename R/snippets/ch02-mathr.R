# 이 장의 계산 몇 가지를 저자의 mathr 패키지 함수로 다시 확인한다
library(mathr)
signif(c(MachEps = MachEps(), R = .Machine$double.eps), 4)   # 2.1 기계 엡실론

# 2.5 의 적분과 미분 (g 는 앞 절의 A exp(-k t), exactI 는 해석해)
print(c(exact = exactI, GQuad8 = GQuad8(g, 0, 20),          # 8점 Gauss 구적법
        Romberg = romb(g, 0, 20, N = 8)), digits = 7)
print(c(Deriv1 = Deriv1(g, 4), by.hand = -k*A*exp(-k*4)), digits = 7)

# 2.5 의 Rosenbrock 함수를 가변거리법(variable metric method)으로 최소화한다
vm <- VMmin(c(-1.2, 1), ros)
signif(c(x = vm$par[1], y = vm$par[2], value = vm$value), 4); vm$FnCount

# 2.4 의 M 을 Cholesky 분해한다 (R 의 chol 과 달리 하삼각 행렬을 반환한다)
round(Chol(M), 4)
