# 화면 대신 파일에 그리려면 장치를 열고, 그린 뒤 반드시 닫는다.
f2 <- file.path(tempdir(), "decay.pdf")
pdf(f2, width = 5, height = 3.2)              # 크기는 인치 단위
par(mar = c(4.2, 4.2, 1, 1))
curve(decay(x), 0, 12, las = 1, bty = "l", xlab = "t (h)", ylab = "C (ng/mL)")
dev.off()                                     # 닫아야 파일이 완성된다
c(created = file.exists(f2), still.open = length(dev.list()) > 0)

# 이 책의 그림은 모두 이 방식으로 R/build.R 이 figures/*.pdf 에 고정한다.
# pdf 는 벡터라 확대해도 깨지지 않는다. 논문·보고서 그림은 png 보다 pdf 가 낫다.
