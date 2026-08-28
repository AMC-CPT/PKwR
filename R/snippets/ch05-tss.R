# 지속정주시 항정상태 도달 정도: 반감기의 n 배가 지난 시점의 Css 대비 비율
n.half <- c(1, 2, 3, 4, 5, 6, 7)
data.frame(half.lives = n.half,
           pct.of.Css = round(100*(1 - 0.5^n.half), 2))
