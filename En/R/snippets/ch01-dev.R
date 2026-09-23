# To draw to a file instead of the screen, open a device, draw, and always close it.
f2 <- file.path(tempdir(), "decay.pdf")
pdf(f2, width = 5, height = 3.2)              # size in inches
par(mar = c(4.2, 4.2, 1, 1))
curve(decay(x), 0, 12, las = 1, bty = "l", xlab = "t (h)", ylab = "C (ng/mL)")
dev.off()                                     # the file is complete only after closing
c(created = file.exists(f2), still.open = length(dev.list()) > 0)

# Every figure in this book is fixed this way by R/build.R into figures/*.pdf.
# pdf is vector graphics and stays sharp when enlarged. For papers and reports, pdf
# beats png.
