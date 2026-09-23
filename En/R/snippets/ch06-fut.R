# If hypoalbuminemia doubles fu (low-extraction drug, constant infusion assumed):
# the total concentration halves, the "unbound" concentration stays the same.
prot <- function(fu, fut = 0.5, Vp = 3, Vt = 39, CLint = 30, R0 = 10) {
  CL   <- fu*CLint                       # low extraction: CLH ~ fu * CLint
  Ctot <- R0/CL                          # steady-state total concentration (mg/L)
  c(fu = fu, CL = CL, Css.total = Ctot, Css.free = fu*Ctot,
    Vd = Vp + Vt*fu/fut)                 # Vd = Vp + Vt fu/fut
}
round(rbind(normal = prot(fu = 0.1), low.albumin = prot(fu = 0.2)), 3)
