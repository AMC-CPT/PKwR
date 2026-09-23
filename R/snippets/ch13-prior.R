# 사전(prior): 11장에서 Theoph 자료로 얻은 FOCE-I 최종 추정치를 그대로 쓴다.
# theophylline 경구 1구획: KA, V, K (겉보기 경구 파라미터, F 는 V 와 CL 에 흡수)
TH <- c(KA = 1.4950, V = 32.4770, K = 0.0872)
OM <- matrix(c( 0.4363, 0.0573, -0.0067,            # eta 공분산 (full block)
                0.0573, 0.0199,  0.0117,
               -0.0067, 0.0117,  0.0206), 3, 3)
SG <- c(prop = 0.0175, add = 0.0787)                # 잔차 분산 (비례, 가법)

ipred <- function(eta, t, D) {                      # 개인 예측 (Bateman)
  ka <- TH[[1]]*exp(eta[1]); v <- TH[[2]]*exp(eta[2]); k <- TH[[3]]*exp(eta[3])
  D/v*ka/(ka - k)*(exp(-k*t) - exp(-ka*t))
}
round(c(CL.pop = TH[[2]]*TH[[3]], t.half.pop = log(2)/TH[[3]],
        IIV.CV = 100*sqrt(exp(diag(OM)) - 1)), 2)
