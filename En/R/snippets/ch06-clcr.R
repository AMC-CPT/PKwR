# Cockcroft-Gault formula for the estimated creatinine clearance (mL/min)
CLcr <- function(age, weight, Scr, sex = c("male", "female")) {
  sex <- match.arg(sex)
  v <- (140 - age)*weight/(72*Scr)
  if (sex == "female") v*0.85 else v
}

# Normal serum creatinine (1.0 mg/dL), yet CLcr is much lower in the old and light.
data.frame(case = c("30 y, 70 kg, male", "68 y, 60 kg, male", "68 y, 50 kg, female"),
           Scr  = 1.0,
           CLcr = round(c(CLcr(30, 70, 1.0), CLcr(68, 60, 1.0),
                          CLcr(68, 50, 1.0, "female")), 1))
