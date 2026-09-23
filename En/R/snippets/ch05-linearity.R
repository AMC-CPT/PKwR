# Linear: Css = D/CL, directly proportional to dose.
# Nonlinear (saturable metabolism): at steady state
#   dosing rate = Vmax*Css/(Km + Css), so
#                    Css = Km*D/(Vmax - D)   (a steady state exists only for D < Vmax)
Css.lin <- function(D, CL) D/CL
Css.mm  <- function(D, Vmax, Km) ifelse(D < Vmax, Km*D/(Vmax - D), NA)

Vmax <- 500; Km <- 5                # mg/day, mg/L  (phenytoin-like)
CL0  <- Vmax/Km                     # clearance in the low-dose limit = 100 L/day

D <- seq(0, 480, 2)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
plot(D, Css.lin(D, CL0), type = "l", las = 1, bty = "l",
     xlab = "Daily Dose (mg)", ylab = "Steady State Conc (mg/L)",
     main = "Linear Pharmacokinetics")
plot(D, Css.mm(D, Vmax, Km), type = "l", las = 1, bty = "l",
     xlab = "Daily Dose (mg)", ylab = "Steady State Conc (mg/L)",
     main = "Nonlinear Pharmacokinetics")

# At low doses the two models nearly coincide; at high doses they diverge widely.
Dose <- c(100, 200, 300, 400)
data.frame(Dose,
           Css.linear    = round(Css.lin(Dose, CL0), 2),
           Css.nonlinear = round(Css.mm(Dose, Vmax, Km), 2),
           ratio         = round(Css.mm(Dose, Vmax, Km)/Css.lin(Dose, CL0), 2))
