# R's basic unit is the vector, not the scalar. Operations act on all elements at once.
x <- c(2.4, 5.1, 7.8, 3.3, 9.0)
c(length = length(x), sum = sum(x), mean = mean(x))
round(x/max(x), 3)                            # vector divided by scalar: recycling

# Two ways to make regular vectors
seq(0, 12, by = 3)
rep(c("A", "B"), times = 3)

# Indexing: by position, by logical, by name
x[2]; x[c(1, 5)]; x[-1]                       # negative means 'except'
x[x > 5]                                      # logical indexing is the most common
names(x) <- c("a", "b", "c", "d", "e"); x[["c"]]

# Missing values propagate. Each function needs its handling stated explicitly.
y <- c(1, NA, 3)
c(sum.default = sum(y), sum.narm = sum(y, na.rm = TRUE), n.missing = sum(is.na(y)))
