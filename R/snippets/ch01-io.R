# 자료는 대개 파일에서 온다. csv 가 가장 다루기 쉽고 어디서나 열린다.
f <- file.path(tempdir(), "pk.csv")
write.csv(d2, f, row.names = FALSE, quote = FALSE)
cat(readLines(f, n = 3), sep = "\n")          # 파일의 앞 세 줄

# 읽을 때는 결측 표기를 명시한다. 빈 칸과 마침표를 결측으로 읽지 않으면
# 그 열이 통째로 문자형이 되고, 뒤의 계산이 조용히 어긋난다.
d3 <- read.csv(f, na.strings = c("", ".", "NA"))
str(d3)
c(same.rows = nrow(d3) == nrow(d2), same.values = isTRUE(all.equal(d3$DV, d2$DV)))

# 경로 구분자는 윈도우에서도 '/' 를 쓴다: "C:/Study/pk.csv"
# 스크립트에는 절대경로 대신 프로젝트 기준 상대경로를 쓴다: "data/pk.csv"
basename(f)
