# Cockcroft-Gault 공식으로 크레아티닌 청소율을 추정한다 (mL/min)
CLcr <- function(age, weight, Scr, sex = c("male", "female")) {
  sex <- match.arg(sex)
  v <- (140 - age)*weight/(72*Scr)
  if (sex == "female") v*0.85 else v
}

# 혈청 크레아티닌이 정상 범위(1.0 mg/dL)여도 고령·저체중이면 크게 낮다.
data.frame(case = c("30세 70 kg 남", "68세 60 kg 남", "68세 50 kg 여"),
           Scr  = 1.0,
           CLcr = round(c(CLcr(30, 70, 1.0), CLcr(68, 60, 1.0),
                          CLcr(68, 50, 1.0, "female")), 1))
