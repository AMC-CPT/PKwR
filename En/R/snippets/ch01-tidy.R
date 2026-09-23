# sort returns the values, order the positions; to sort a table, always use order.
d2 <- data.frame(ID = c(3, 1, 2, 1, 3, 2), TIME = c(2, 2, 0, 0, 0, 2))
d2$DV <- round(decay(d2$TIME, A = 70 + 10*d2$ID), 2)
d2 <- d2[order(d2$ID, d2$TIME), ]             # ID ascending, TIME within ID
row.names(d2) <- NULL; d2

# Attaching covariates: row order and count must be kept, so match instead of merge
cov1 <- data.frame(ID = c(2, 1, 3), WT = c(75, 62, 88))
d2$WT <- cov1$WT[match(d2$ID, cov1$ID)]       # the left table's order is preserved
stopifnot(nrow(d2) == 6, !anyNA(d2$WT))
head(d2, 3)

# Long -> wide (the T/R pairing of the crossover design in Chapter 14 looks like this)
reshape(d2[, c("ID", "TIME", "DV")], idvar = "ID", timevar = "TIME",
        direction = "wide")
