# data.frame: a table whose columns may differ in type; all data in this book are one.
d1 <- data.frame(ID = rep(1:3, each = 4), TIME = rep(c(0, 2, 4, 8), 3),
                 DV = round(decay(rep(c(0, 2, 4, 8), 3),
                                  A = rep(c(90, 100, 120), each = 4)), 2))
head(d1, 5); dim(d1)

# Columns by $ or [[ ]], rows by a logical index
d1$DV[1:3]
d1[d1$ID == 2 & d1$TIME > 0, ]

# Summaries: for by-group computation, tapply or aggregate is convenient
round(tapply(d1$DV, d1$ID, max), 2)
aggregate(DV ~ ID, d1, function(v) round(c(max = max(v), min = min(v)), 2))
