# A for loop works, but the apply family is shorter and the result's shape is explicit
ks <- c(0.1, 0.25, 0.5)
out <- numeric(length(ks))                    # for: make the container first
for (i in seq_along(ks)) out[i] <- decay(4, k = ks[i])
round(out, 3)

round(sapply(ks, function(k) decay(4, k = k)), 3)      # sapply: returns a vector
str(lapply(ks, function(k) decay(c(0, 4), k = k)))     # lapply: returns a list

# Matrix row/column summaries: apply; binding a list into one table: do.call(rbind, .)
m <- sapply(ks, function(k) decay(c(0, 2, 4), k = k))
round(apply(m, 2, sum), 3)                    # 2 = by column
do.call(rbind, lapply(ks, function(k) c(k = k, y4 = round(decay(4, k = k), 3))))

# To repeat the same computation many times with random numbers, use replicate
set.seed(20260828)
round(quantile(replicate(1000, mean(rnorm(5))), c(0.025, 0.975)), 3)
