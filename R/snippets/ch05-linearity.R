# 선형: Css = D/CL 로 용량에 정비례.
# 비선형(포화 대사): 항정상태에서 투여속도 = Vmax*Css/(Km + Css) 이므로
#                    Css = Km*D/(Vmax - D)   (D < Vmax 에서만 항정상태가 있다)
Css.lin <- function(D, CL) D/CL
Css.mm  <- function(D, Vmax, Km) ifelse(D < Vmax, Km*D/(Vmax - D), NA)

Vmax <- 500; Km <- 5                # mg/day, mg/L  (phenytoin 유사)
CL0  <- Vmax/Km                     # 저용량 극한의 청소율 = 100 L/day

D <- seq(0, 480, 2)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
plot(D, Css.lin(D, CL0), type = "l", las = 1, bty = "l",
     xlab = "Daily Dose (mg)", ylab = "Steady State Conc (mg/L)",
     main = "Linear Pharmacokinetics")
plot(D, Css.mm(D, Vmax, Km), type = "l", las = 1, bty = "l",
     xlab = "Daily Dose (mg)", ylab = "Steady State Conc (mg/L)",
     main = "Nonlinear Pharmacokinetics")

# 저용량에서는 두 모형이 거의 같지만, 고용량에서 크게 벌어진다.
Dose <- c(100, 200, 300, 400)
data.frame(Dose,
           Css.linear    = round(Css.lin(Dose, CL0), 2),
           Css.nonlinear = round(Css.mm(Dose, Vmax, Km), 2),
           ratio         = round(Css.mm(Dose, Vmax, Km)/Css.lin(Dose, CL0), 2))
