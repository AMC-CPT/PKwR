# 표준오차는 목적함수의 곡률에서 나온다. 골짜기가 좁으면 정밀하고 넓으면 아니다.
library(numDeriv)
pe  <- o3[1:2]                                # ELS 추정치 (A, k)
H   <- hessian(function(p) els(c(p, o3[3])), pe)
Cov <- 2*solve(H)                             # OFV = -2logL 이므로 2를 곱한다
se  <- sqrt(diag(Cov))
m <- rbind(PE = pe, SE = se, `RSE%` = 100*se/pe,
           LL = pe - 1.96*se, UL = pe + 1.96*se)
colnames(m) <- c("A", "k"); round(m, 4)
round(setNames(as.data.frame(cov2cor(Cov)), c("A", "k")), 3)                        # 추정치끼리의 상관

# 유도 파라미터의 SE: 델타법. h = log2/k 이면 dh/dk = -log2/k^2
h  <- log(2)/pe[2]
se.h <- abs(-log(2)/pe[2]^2)*se[2]
round(c(half.life = h, SE = se.h, LL = h - 1.96*se.h, UL = h + 1.96*se.h), 4)
