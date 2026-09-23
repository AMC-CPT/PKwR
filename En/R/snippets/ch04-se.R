# SEs come from the curvature of the objective: narrow valley precise, wide one not.
library(numDeriv)
pe  <- o3[1:2]                                # ELS estimates (A, k)
H   <- hessian(function(p) els(c(p, o3[3])), pe)
Cov <- 2*solve(H)                             # multiply by 2 because OFV = -2logL
se  <- sqrt(diag(Cov))
m <- rbind(PE = pe, SE = se, `RSE%` = 100*se/pe,
           LL = pe - 1.96*se, UL = pe + 1.96*se)
colnames(m) <- c("A", "k"); round(m, 4)
round(setNames(as.data.frame(cov2cor(Cov)), c("A", "k")), 3)  # estimate correlation

# SE of a derived parameter: delta method. If h = log2/k then dh/dk = -log2/k^2
h  <- log(2)/pe[2]
se.h <- abs(-log(2)/pe[2]^2)*se[2]
round(c(half.life = h, SE = se.h, LL = h - 1.96*se.h, UL = h + 1.96*se.h), 4)
