# 저알부민혈증으로 fu 가 두 배가 되면 (저추출률 약물, 지속정주 가정):
# 총농도는 반토막, "유리"농도는 그대로다.
prot <- function(fu, fut = 0.5, Vp = 3, Vt = 39, CLint = 30, R0 = 10) {
  CL   <- fu*CLint                       # 저추출: CLH ~ fu * CLint
  Ctot <- R0/CL                          # 항정상태 총농도 (mg/L)
  c(fu = fu, CL = CL, Css.total = Ctot, Css.free = fu*Ctot,
    Vd = Vp + Vt*fu/fut)                 # Vd = Vp + Vt fu/fut
}
round(rbind(normal = prot(fu = 0.1), low.albumin = prot(fu = 0.2)), 3)
