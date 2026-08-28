# =====================================================================
#  R/build.R  -  regenerate all frozen R output and figures
#  Run from the repository root:   Rscript R/build.R
#  Add one freeze(...) line per snippet as chapters are written.
#  전체 실행 시간: 약 12-15분 (11장의 FOCE-I, RPT, bootstrap 이 대부분).
# =====================================================================
if (!file.exists("PKwR.tex"))
  stop("Run from the repository root (where PKwR.tex lives).")

dir.create("output",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
source("R/_freeze.R")

message("Freezing R output/figures ...")

## ---- Chapter 1: R, Git, Markdown, AI ----------------------------------
#  자체 세션. ch01-fun 이 decay 를 정의하고 뒤 스니펫이 이어 쓴다.
new_session()
freeze("ch01-work",    digits = 4)
freeze("ch01-vec",     digits = 4)
freeze("ch01-type",    digits = 4)
freeze("ch01-fun",     digits = 4)
freeze("ch01-flow",    digits = 4)
freeze("ch01-df",      digits = 4)
freeze("ch01-matlist", digits = 4)
freeze("ch01-apply",   digits = 4)
freeze("ch01-tidy",    digits = 4)
freeze("ch01-io",      digits = 4)
freeze("ch01-plot",    fig = TRUE, fig.w = 7.4, fig.h = 2.9, digits = 4)
freeze("ch01-dev",     digits = 4)
freeze("ch01-seed",    digits = 4)

## ---- Chapter 2: 수학 기초 -----------------------------------------------
#  자체 세션. ch02-exp 가 k/A/tt/yy 를, ch02-bateman 이 ka/ke 를,
#  ch02-int 가 g 와 exactI 를 정의하고 뒤 스니펫이 이어 쓴다.
new_session()
freeze("ch02-float",   digits = 4)
freeze("ch02-exp",     fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch02-ode1",    digits = 4)
freeze("ch02-bateman", digits = 4)
freeze("ch02-odesys",  digits = 4)
freeze("ch02-mat",     digits = 4)
freeze("ch02-lin",     digits = 4)
freeze("ch02-int",     digits = 4)
freeze("ch02-quad",    digits = 4)
freeze("ch02-diff",    digits = 4)
freeze("ch02-fd",      fig = TRUE, fig.w = 5.6, fig.h = 3.4, digits = 4)
freeze("ch02-root",    digits = 4)
freeze("ch02-min",     digits = 4)
freeze("ch02-taylor",  fig = TRUE, fig.w = 5.4, fig.h = 3.4, digits = 4)

## ---- Chapter 3: 통계 기초 ---------------------------------------------
#  자체 세션. ch03-dist 가 mu/sg 를, ch03-summary 가 x 와 gm/gcv 를,
#  ch03-lik 가 y 를 정의하고 뒤 스니펫이 이어 쓴다. set.seed(20260828).
new_session()
freeze("ch03-dist",    fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch03-summary", digits = 4)
freeze("ch03-dpqr",    digits = 4)
freeze("ch03-clt",     fig = TRUE, fig.w = 7.2, fig.h = 2.9, digits = 4)
freeze("ch03-lik",     fig = TRUE, fig.w = 5.6, fig.h = 3.4, digits = 4)
freeze("ch03-ci",      fig = TRUE, fig.w = 5.4, fig.h = 3.6, digits = 4)
freeze("ch03-li",      fig = TRUE, fig.w = 5.8, fig.h = 3.5, digits = 4)
freeze("ch03-test",    fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch03-lrt",     fig = TRUE, fig.w = 5.6, fig.h = 3.2, digits = 4)
freeze("ch03-rng",     digits = 4)
freeze("ch03-mvn",     digits = 4)
freeze("ch03-boot",    digits = 4)
freeze("ch03-lm",      digits = 4)

## ---- Chapter 4: 비선형 회귀 -------------------------------------------
#  자체 세션. ch04-sim 이 d4 를, ch04-obj 가 sse 와 fit.ols 를, ch04-gn 이 Jac 을,
#  ch04-weight 가 o1/o3 와 els 를, ch04-prof 가 pe4/se4/dg/tK/tc 를 정의하고
#  뒤 스니펫이 이어 쓴다. 자체 set.seed(20260828).
new_session()
freeze("ch04-sim",    fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch04-obj",    fig = TRUE, fig.w = 5.4, fig.h = 3.6, digits = 4)
freeze("ch04-gn",     digits = 4)
freeze("ch04-weight", fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch04-nlr",    digits = 4)
freeze("ch04-init",   digits = 4)
freeze("ch04-plin",   digits = 4)
freeze("ch04-se",     digits = 4)
freeze("ch04-prof",   fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch04-repar",  digits = 4)
freeze("ch04-diag",   fig = TRUE, fig.w = 7.2, fig.h = 2.8, digits = 4)
freeze("ch04-run",    digits = 4)
freeze("ch04-sel",    digits = 4)
freeze("ch04-disc",   digits = 4)

## ---- Chapter 5: 약동학 입문 -----------------------------------------
new_session()
# 농도 함수 (이후 스니펫이 계속 쓴다)
freeze("ch05-pkfun",       digits = 4)
freeze("ch05-conc-effect", fig = TRUE, fig.w = 6.0, fig.h = 3.4)
freeze("ch05-routes",      fig = TRUE, fig.w = 7.2, fig.h = 2.7)
freeze("ch05-logconc",     fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
# 약동학 파라미터
freeze("ch05-params",      digits = 4)
# 반복투여·지속정주
freeze("ch05-multidose",   fig = TRUE, fig.w = 6.4, fig.h = 3.6)
freeze("ch05-tss")
freeze("ch05-loading",     digits = 4)
freeze("ch05-digoxin",     fig = TRUE, fig.w = 6.4, fig.h = 3.6)
freeze("ch05-fluctuation", fig = TRUE, fig.w = 6.4, fig.h = 3.6, digits = 4)
# 선형성과 비선형성
freeze("ch05-linearity",   fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch05-tdep",        fig = TRUE, fig.w = 6.2, fig.h = 3.4, digits = 4)
# 용량 비례성
freeze("ch05-dp-sim")
freeze("ch05-dp-nca",      digits = 5)
freeze("ch05-dp-power",    digits = 4)
freeze("ch05-dp-crit",     digits = 4)
freeze("ch05-dp-plot",     fig = TRUE, fig.w = 7.0, fig.h = 3.4)

## ---- Chapter 6: ADME ------------------------------------------------
#  new_session() 을 두지 않는다: ch06-flipflop 이 5장에서 정의한 Cpo 를 쓴다.
#  (책에서 같은 함수를 두 번 싣지 않기 위한 의도적 의존이다. 6장 앞에 다른
#   장을 끼워 넣을 때는 이 순서를 함께 옮겨야 한다.)
# 흡수: 이온화와 pH, flip-flop
freeze("ch06-ion",         fig = TRUE, fig.w = 6.0, fig.h = 3.4, digits = 4)
freeze("ch06-flipflop",    fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
# 분포: 단백결합 변화의 귀결
freeze("ch06-fut",         digits = 4)
freeze("ch06-bind",        fig = TRUE, fig.w = 5.8, fig.h = 3.5, digits = 4)
# 대사: Michaelis-Menten 과 well-stirred model
freeze("ch06-mm")
freeze("ch06-hep",         fig = TRUE, fig.w = 5.8, fig.h = 3.5, digits = 4)
# 배설: 신청소율 판독, Cockcroft-Gault, fe 용량조절
freeze("ch06-clr",         digits = 4)
freeze("ch06-clcr")
freeze("ch06-fe",          digits = 4)

## ---- Chapter 7: 구획 분석 --------------------------------------------
#  자체 세션 시작. ch07-c2iv 가 th(거시 상수)와 C2iv 를, ch07-sim2c 가
#  dat2 를 정의하고 이후 스니펫이 이어 쓴다. ch07-sim2c 는 자체
#  set.seed(20260827) 를 쓴다. 새 스니펫(salt, c3iv, metab)은 8장이
#  이어받는 객체(C2iv, th, dat2, D, CL, tobs)를 덮어쓰지 않는 이름만 쓴다.
new_session()
freeze("ch07-salt",   digits = 4)
freeze("ch07-c2iv",   digits = 4)
freeze("ch07-vdtime", fig = TRUE, fig.w = 6.0, fig.h = 3.3)
freeze("ch07-ehc",     fig = TRUE, fig.w = 6.2, fig.h = 3.5, digits = 4)
freeze("ch07-c3iv",   fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch07-sim2c",  fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch07-strip",  digits = 4)
freeze("ch07-fit",    digits = 4)
freeze("ch07-gof",    fig = TRUE, fig.w = 7.0, fig.h = 3.2)
freeze("ch07-derive", digits = 4)
freeze("ch07-metab",  fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)

## ---- Chapter 8: 비구획 분석 (NCA) ------------------------------------
#  new_session() 을 두지 않는다: 8장은 7장의 dat2, C2iv, th, D, CL,
#  tobs 를 그대로 분석한다(같은 자료를 두 방법으로 보는 의도적 의존).
#  ch08-front/pred 는 ch08-moments 가 만든 C0, a0, AUC.inf 를 이어 쓰고,
#  ch08-urine 은 ch08-pred 의 AUC.lst 를 쓴다(자체 set.seed(20260827)).
freeze("ch08-auc",     digits = 5)
freeze("ch08-trapfig", fig = TRUE, fig.w = 7.0, fig.h = 3.2)
freeze("ch08-lambdaz", fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch08-moments", digits = 4)
freeze("ch08-front",   digits = 4)
freeze("ch08-pred",    digits = 4)
freeze("ch08-snca",    digits = 4)
freeze("ch08-tblnca",  digits = 3)
freeze("ch08-urine",   fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch08-ss",      digits = 4)
freeze("ch08-bailer",  digits = 4)

## ---- Chapter 9: PBPK -------------------------------------------------
#  자체 세션. ch09-phys/drug 가 파라미터를, ch09-pbpk 가 dydt 모형과
#  iv 시뮬레이션을 정의하고 이후 스니펫(check/oral/ddi)이 이어 쓴다.
new_session()
freeze("ch09-phys",  digits = 4)
freeze("ch09-drug",  digits = 4)
freeze("ch09-pbpk",  fig = TRUE, fig.w = 6.4, fig.h = 3.6)
freeze("ch09-check", digits = 4)
freeze("ch09-oral",  fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch09-perm",   fig = TRUE, fig.w = 6.0, fig.h = 3.5, digits = 4)
freeze("ch09-sens",   fig = TRUE, fig.w = 5.8, fig.h = 3.4, digits = 4)
freeze("ch09-ddi",   digits = 4)
freeze("ch09-extrap", digits = 4)

## ---- Chapter 10: 집단 약동학 ------------------------------------------
#  자체 세션. Theoph 를 NONMEM 형식으로 만들고 nmw 로 FO -> FOCE-I 추정.
#  ch10-foce 의 EstStep 이 약 45초, ch10-focecov 의 CovStep 이 약 20초.
#  ch10-ctl 은 NONMEM 제어파일 예문이라 freeze 하지 않는다(\rcode 전용).
new_session()
freeze("ch10-data",    digits = 4)
freeze("ch10-spag",    fig = TRUE, fig.w = 6.0, fig.h = 3.6)
freeze("ch10-pred")
freeze("ch10-init")
freeze("ch10-est",     digits = 4)
freeze("ch10-cov",     digits = 4)
freeze("ch10-posthoc", digits = 4)
freeze("ch10-foce",    digits = 4)
freeze("ch10-focecov", digits = 4)
freeze("ch10-ebe",     digits = 4)
freeze("ch10-obj",     digits = 4)
freeze("ch10-wres",    fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch10-blq",     fig = TRUE, fig.w = 6.6, fig.h = 3.9, digits = 4)
freeze("ch10-design",  digits = 4)

## ---- Chapter 11: 집단 모형의 진단과 검증 -------------------------------
#  new_session() 을 두지 않는다: 11장은 10장의 적합(DATA, PRED, TH, OM,
#  SG, EBE, r.fo, r.foce, cov.fo 등)을 그대로 진단·검증한다.
#  ch11-wt(FOCE-I 재적합, 약 90초) 이후 nmw 내부 환경은 공변량 모형
#  상태가 되므로 base 진단(tab ... vpc)은 반드시 그 앞에 둔다.
#  ch11-rpt 는 FO 50회(약 4분), ch11-boot 는 FO 100회(약 2분)를 돌린다.
freeze("ch11-tab",     digits = 4)
freeze("ch11-iofv",    digits = 4)
freeze("ch11-gof",     fig = TRUE, fig.w = 7.0, fig.h = 3.5)
freeze("ch11-resid",   fig = TRUE, fig.w = 6.6, fig.h = 5.4, digits = 3)
freeze("ch11-etadist", fig = TRUE, fig.w = 7.0, fig.h = 2.8)
freeze("ch11-etacov",  fig = TRUE, fig.w = 7.0, fig.h = 2.8)
freeze("ch11-vpc",     fig = TRUE, fig.w = 6.4, fig.h = 3.6)
freeze("ch11-wt",      digits = 4)
freeze("ch11-forest",  fig = TRUE, fig.w = 6.0, fig.h = 3.2, digits = 4)
freeze("ch11-rpt",     fig = TRUE, fig.w = 6.0, fig.h = 3.3, digits = 4)
freeze("ch11-boot",    digits = 4)

## ---- Chapter 12: EBE 를 이용한 TDM -------------------------------------
#  자체 세션(10-11장 세션을 이어받지 않는다). ch12-prior 가 사전(TH/OM/SG,
#  10장 FOCE-I 최종 추정치를 하드코딩)과 ipred 를, ch12-pt 가 환자 자료를
#  정의하고 이후 스니펫이 이어 쓴다. ch12-pt 는 자체 set.seed(20260827).
#  ch12-nmw 는 nmw 를 새로 초기화해 PostHocEta 로 검산한다(EstStep 없음).
new_session()
freeze("ch12-mm",     fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch12-prior",  digits = 4)
freeze("ch12-pt",     digits = 4)
freeze("ch12-map",    fig = TRUE, fig.w = 6.4, fig.h = 3.8, digits = 4)
freeze("ch12-post",   digits = 4)
freeze("ch12-nmw",    digits = 4)
freeze("ch12-design", fig = TRUE, fig.w = 5.8, fig.h = 3.4, digits = 4)
freeze("ch12-dose",   fig = TRUE, fig.w = 6.4, fig.h = 3.6, digits = 4)
freeze("ch12-load",   fig = TRUE, fig.w = 6.2, fig.h = 3.4, digits = 4)
freeze("ch12-auc",    digits = 4)
freeze("ch12-engine", digits = 4)
freeze("ch12-engchk", digits = 4)
freeze("ch12-fit2",   digits = 4)
freeze("ch12-fit2r",  digits = 4)
freeze("ch12-swap",   digits = 4)
freeze("ch12-ssq",    digits = 4)

## ---- Chapter 13: 생물학적동등성 -----------------------------------------
#  자체 세션. sasLM, BE, PowerTOST, nlme 를 쓴다. ch13-sim 이 d13(2x2
#  자료)을, ch13-gsim 이 gd(다입원군 자료)를, ch13-tsd1 이 st1/n1/a2/
#  mse1/n.new 를 정의하고 뒤 스니펫이 이어 쓴다. 시뮬레이션 스니펫은
#  각자 set.seed(20260827) 를 쓴다(전부 새 수치이며 실제 사례 아님).
new_session()
freeze("ch13-sim",     digits = 4)
freeze("ch13-fig",     fig = TRUE, fig.w = 7.0, fig.h = 3.2)
freeze("ch13-anova",   digits = 4)
freeze("ch13-test2x2", digits = 4)
freeze("ch13-ss",      digits = 4)
freeze("ch13-gsim",    digits = 4)
freeze("ch13-gfit",    digits = 4)
freeze("ch13-tsd1",    digits = 4)
freeze("ch13-tsd2",    digits = 4)
freeze("ch13-scaled",  fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)

## ---- Chapter 14: 임상개발의 약동학 -----------------------------------
#  자체 세션. 다른 장의 객체를 쓰지 않는다.
#  ch14-expostat 은 대표성 있는 표본(산술평균 > 기하평균 ~ 모집단 중앙값)을
#  위해 seed = 6 을 쓴다.
new_session()
freeze("ch14-allometry", fig = TRUE, fig.w = 5.6, fig.h = 3.6, digits = 4)
freeze("ch14-dedrick",     fig = TRUE, fig.w = 7.0, fig.h = 3.3, digits = 4)
freeze("ch14-expostat",  seed = 6, fig = TRUE, fig.w = 6.0, fig.h = 3.3, digits = 4)
freeze("ch14-super",     fig = TRUE, fig.w = 6.4, fig.h = 3.3)
freeze("ch14-hepatic")
freeze("ch14-effcomp",   fig = TRUE, fig.w = 7.0, fig.h = 3.2)
freeze("ch14-idr",       fig = TRUE, fig.w = 6.0, fig.h = 3.3, digits = 4)
freeze("ch14-iiv",       fig = TRUE, fig.w = 6.0, fig.h = 3.6)

## ---- Chapter 15: 약력학 ---------------------------------------------
#  자체 세션. ch15-emax 가 Emax.model·sEmax.model 을 정의하고
#  ch15-antag·ch15-therange 가 그것을 쓴다. wnl(bind)을 쓰며,
#  시뮬레이션 스니펫은 set.seed(20260828)을 쓴다.
new_session()
freeze("ch15-bind",        fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch15-emax",        fig = TRUE, fig.w = 7.0, fig.h = 3.2)
freeze("ch15-antag",       fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch15-therange",    fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch15-logit",       fig = TRUE, fig.w = 5.8, fig.h = 3.5, digits = 4)
freeze("ch15-duration",    fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)

## ---- Chapter 16: PK-PD 모델링 ---------------------------------------
#  자체 세션(15장 세션을 이어받지 않는다). ch16-idr4 가 deSolve 를 먼저
#  올리고 base·kill 이 그것을 쓴다. drt 는 wnl 을 쓰며 자체
#  set.seed(20260828)을 둔다.
new_session()
freeze("ch16-idr4",        fig = TRUE, fig.w = 6.6, fig.h = 5.0, digits = 4)
freeze("ch16-base",        fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch16-tol",         fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch16-kill",        fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch16-disprog",     fig = TRUE, fig.w = 6.0, fig.h = 3.6, digits = 4)
freeze("ch16-drt",         fig = TRUE, fig.w = 7.0, fig.h = 3.2, digits = 4)
freeze("ch16-syn",         fig = TRUE, fig.w = 7.0, fig.h = 3.4, digits = 4)

message("Done.")
