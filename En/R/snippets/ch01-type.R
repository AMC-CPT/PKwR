# A vector holds one type only. Mixing types silently promotes to the wider one.
c(num = class(c(1, 2)), mixed = class(c(1, "2")), lgl = class(c(TRUE, 1)))
as.numeric(c("1.5", "abc"))                   # NA if it cannot convert (with a warning)

# Four special values with different meanings. Log axes and BLQ data always meet them.
z <- c(0/0, 1/0, NA, 3)
rbind(is.na = is.na(z), is.nan = is.nan(z), is.finite = is.finite(z))
c(log.of.0 = log(0), zero.times.Inf = 0*Inf)

# factor: turns a numerically coded category into levels; Chapter 14's ANOVA uses it.
grp <- c(1, 2, 3, 1, 2, 3); yy <- c(5, 7, 12, 6, 8, 11)
c(as.number = df.residual(lm(yy ~ grp)),      # becomes a regression with one slope
  as.factor = df.residual(lm(yy ~ factor(grp))))
