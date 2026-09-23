# Data usually come from files. csv is the easiest to handle and opens anywhere.
f <- file.path(tempdir(), "pk.csv")
write.csv(d2, f, row.names = FALSE, quote = FALSE)
cat(readLines(f, n = 3), sep = "\n")          # the first three lines of the file

# When reading, state the missing-value codes. If blanks and periods are not read
# as missing, the whole column becomes character and later steps quietly go wrong.
d3 <- read.csv(f, na.strings = c("", ".", "NA"))
str(d3)
c(same.rows = nrow(d3) == nrow(d2), same.values = isTRUE(all.equal(d3$DV, d2$DV)))

# Use '/' as the path separator even on Windows: "C:/Study/pk.csv"
# In scripts use project-relative paths, not absolute ones: "data/pk.csv"
basename(f)
