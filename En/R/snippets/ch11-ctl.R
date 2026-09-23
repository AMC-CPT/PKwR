$PROB THEOPHYLLINE ORAL 1-COMP    P:ROOT    F:BASE
;  Code : 100 = 1-comp, 1st-order absorption, combined error
;  ETA  : KA, V, K (full block)
$INPUT ID TIME DV BWT DOSE
$DATA  THEO.CSV IGNORE=@
$ABBR  DERIV2=NO
$PRED
  KA = THETA(1) * EXP(ETA(1))
  V  = THETA(2) * EXP(ETA(2))
  K  = THETA(3) * EXP(ETA(3))
  F  = DOSE/V*KA/(KA - K)*(EXP(-K*TIME) - EXP(-KA*TIME))
  IPRE = F
  Y  = F + F*EPS(1) + EPS(2)
$THETA
  (0, 2)          ; 1 KA (/hr)
  (0, 50)         ; 2 V  (L)
  (0, 0.1)        ; 3 K  (/hr)
$OMEGA BLOCK(3)
  0.2
  0.1  0.2
  0.1  0.1  0.2
$SIGMA 0.1  0.1   ; proportional, additive
$EST MAX=9999 PRINT=5 METHOD=COND INTER
     NOABORT NOTBT NOOBT NOSBT
$COV UNCOND PRINT=ERS
$TAB ID TIME DV PRED IPRE CWRES FILE=sdtab NOPRINT ONEHEADER
$TAB ID ETAS(1:LAST) FILE=patab NOPRINT ONEHEADER NOAPPEND FIRSTONLY
