# 반코마이신 TDM — 관리자 매뉴얼

앱을 설치·실행·유지보수·검증하는 담당자를 위한 문서. 최종 사용자용 안내는 앱 안의
**도움말** 탭 또는 [help_user.md](help_user.md) 참고. 프로젝트 개요는 [README.md](README.md).

---

## 1. 요구사항

- **R** 4.x (개발·검증 환경: 4.6.1)
- **R 패키지** — 외부 의존은 두 개뿐:

  | 패키지 | 필수/권장 | 용도 |
  |--------|-----------|------|
  | `shiny` | **필수** | 앱 프레임워크(UI/서버). 의존 패키지(`htmltools` 등)는 자동 설치됨 |
  | `commonmark` | **권장** | **도움말(Help) 탭**의 마크다운(`help_user.md`)을 HTML로 렌더. 없으면 원문 텍스트로 표시됨 — 앱 동작은 정상, 표·서식만 밋밋해짐 |

  ```r
  install.packages(c("shiny", "commonmark"))
  # 설치 확인:
  all(c("shiny", "commonmark") %in% rownames(installed.packages()))
  ```

  그 외에는 **base R 만 사용**한다(`stats::optim`, `utils::read.csv`, `graphics`/`grDevices` 등) → 추가 설치 불필요.
  회귀 테스트(`tests/regression.R`)도 base R(`saveRDS`/`readRDS`)만 쓴다.
- 별도 데이터베이스·서버 불필요. 단일 사용자 desktop 실행을 기본 가정.

---

## 2. 실행 방법

**권장 — 폴더로 실행** (Shiny가 작업 디렉터리를 앱 폴더로 잡아 엔진/모델 파일을 항상 찾음):
```r
shiny::runApp("C:/R/TDM-Vanco", port = 4972, host = "127.0.0.1")
```
[RunMe.R](RunMe.R) 을 소스해도 동일. 개별 진입 파일도 직접 실행 가능:
```r
shiny::runApp("C:/R/TDM-Vanco/Start4.R")  # AMC + Inje (모델 선택)
shiny::runApp("C:/R/TDM-Vanco/Start3.R")  # Inje 단일
```
`app.R` = `Start4.R` 와 동일한 표준 진입점.

---

## 3. 파일 구조

```
TDM-Vanco/
├── TDMLIB3.R    검증된 수치 엔진 (전방모델·MAP추정·예측구간·AUC용량·CSV수집)
├── models.R     모집단 모델 단일 출처 (PARSETS, AUC_TARGET, TINF)
├── tdmApp.R     앱 팩토리 tdmApp(models, selected, title, auc_target, tinf, help_md)
├── app.R        표준 진입점 (runApp 폴더용)
├── Start4.R     얇은 wrapper — AMC+Inje
├── Start3.R     얇은 wrapper — Inje 단일
├── RunMe.R      런처
├── help_user.md 사용자 안내 (도움말 탭에 렌더)
├── README.md    프로젝트 개요·변경 이력·저자 결정 항목
├── ADMIN.md     이 문서
└── tests/       회귀 테스트 (합성 데이터)
```

**의존 방향**: 진입 파일 → (`.tdm_root` 탐지) → `TDMLIB3.R` + `models.R` + `tdmApp.R` 소스 → `tdmApp(...)` 호출로 `shinyApp` 객체 반환.

---

## 4. 모집단 모델 추가·수정

[models.R](models.R) 의 `PARSETS` 한 곳만 수정하면 된다. 각 모델은 `list(TH, OM, SG)`:

```r
PARSETS = list(
  AMC  = list(TH = c(CL, V1, V2, Q), OM = <4x4 OMEGA>, SG = <2x2 SIGMA>),
  Inje = list(...),
  NEW  = list(TH = ..., OM = ..., SG = ...)   # 새 모델 추가
)
```
- `TH` = c(CL, V1, V2, Q) 전형값. **CL은 CLcr 스케일**: `CL = TH[1]·CLcr/100·exp(ETA1)` (엔진이 CLcr을 150에서 상한).
- `OM` = OMEGA. 대각이 0인 ETA는 IIV 없음(0 근처로 고정). 화면 라벨의 "N ETAs"는 `sum(diag(OM)>0)` 로 자동 산출.
- `SG` = SIGMA = `matrix(c(비례오차^2, 0, 0, 가법오차^2), 2)`.
  - **주의**: 가법오차(SG[2,2])가 0인 순수 비례오차 모델(예: AMC)은 사전투여(예측=0) 시료에서 잔차분산이 0이 된다. 엔진이 그런 무정보 관측을 자동 제외하도록 처리돼 있으나(§6·§7 참고), 새 모델 추가 시 이 특성을 인지할 것.
- 라디오 기본 선택은 진입 파일의 `selected =` 인자로 지정. 목표 AUC/Tinf 는 `AUC_TARGET`, `TINF`.

**모델을 바꾸면 반드시 회귀 기준선을 다시 만들고(아래), 참조값과 대조할 것.**

---

## 5. 회귀 테스트 (수치 불변 보증)

엔진(`TDMLIB3.R`)이나 `models.R` 을 수정하면 검증된 숫자가 바뀌지 않았는지 확인해야 한다.

**리포 내 합성 데이터 테스트** (실제 환자 데이터 없이 상시 실행 가능):
```r
Rscript tests/regression.R check   # 현재 출력과 기준선 비교, 차이 시 exit 1
Rscript tests/regression.R make    # 의도된 변경 후 기준선 재생성
```
- `tests/fixtures/` 의 합성 환자로 `EBE·SE·COV·IPRED·AUC/용량`을 고정.
- `synthetic_predose_amc.csv` 는 AMC 사전투여-0 케이스를 포함해 해당 수정이 유지됨을 잠근다.
- **PASS 조건**: `max|delta| = 0` (비트 단위 동일).

**전수(실데이터) 회귀** — 참조 코호트가 있을 때 권장. 예: `C:/G/TDM/BAIK1`, `BAIK2` 의 환자별 CSV.
1. 수정 전 엔진(`git show HEAD:TDMLIB3.R`)과 수정 후 엔진으로 각각 모든 파일 × 모든 모델을 돌려 결과를 저장.
2. 두 결과를 대조: 기존에 fit되던 모든 환자는 **비트 동일**, 상태 변화(정상↔오류)만 의도된 것인지 확인.
3. 추가로 `EBE_batch_BAIK1_BAIK2.csv` 의 참조 Inje EBE와 대조하면 엔진이 NONMEM 결과를 재현하는지 확인 가능.

> 참고: 최근 개선에서 위 절차로 356명(BAIK1+BAIK2) × 2모델 전수 대조를 수행했고, 안전 수정, AMC 사전투여-0 수정, 날짜/성별 검증, 엔진 클로저화가 모두 **기존 fit 환자에 대해 비트 단위 동일**, Inje EBE가 NONMEM 배치 참조와 356명 전원 일치(반올림 오차 5e-6)함을 확인함.

**실제 환자 데이터는 리포에 커밋하지 말 것** (프라이버시). 전수 회귀는 로컬에서만.

---

## 6. 문제 해결

| 증상 | 원인 / 조치 |
|------|-------------|
| 실행 시 *Cannot locate TDMLIB3.R / models.R / tdmApp.R* | 앱 폴더가 아닌 곳에서 개별 파일 실행. `runApp("C:/R/TDM-Vanco")` 로 폴더 실행하거나 해당 폴더에서 실행. |
| 포트 사용 중 | `RunMe.R`/`runApp` 의 `port` 변경. 한 번에 하나의 앱만(runApp은 블로킹). |
| 도움말 탭이 원문 텍스트로 보임 | `commonmark` 미설치. `install.packages("commonmark")`. |
| 업로드 오류 메시지(용량-속도 불일치, 다중 ID, CLCR 없음 등) | 입력 형식 문제. [help_user.md](help_user.md) §6 참고. 엔진이 잘못된 데이터를 조용히 처리하지 않고 **명확히 거부**하도록 설계됨. |
| AMC 모델에서 추정 실패했었음 | 사전투여(예측=0) 시료에서 발생하던 문제로, 현재 엔진은 해당 무정보 관측을 자동 제외해 fit함(§7). |

---

## 7. 유지보수 시 주의 (검증된 코어)

- `TDMLIB3.R` 의 수치 코어(전방모델, MAP 목적함수, 정보행렬, AUC/정상상태 공식)는 **검증됨**. 수정 시 §5 회귀로 비트 단위 불변을 증명할 것.
- `EBE` 는 최적화 목적함수(`ObjEta`)를 **지역 클로저**로 생성한다(전역 가변 scratch env 없음). 각 호출이 자기 데이터/사전분포를 캡처하므로 **재진입 안전** — 다중 사용자·비동기(`future`/`promises`)·병렬 fitting에서도 서로 간섭하지 않는다.
- **저자 결정 반영 결과**(투여간격 스냅, 겹치는 주입, 날짜 형식, 성별 코딩, 엔진 클로저화)와 남은 선택 항목은 [README.md](README.md) 하단 표 참고. 검증된 코어 관련 변경은 반드시 §5 회귀로 대조.

---

## 8. 배포·보안 참고

- 기본 가정은 **단일 사용자 로컬 실행**(`host="127.0.0.1"`).
- 다중 사용자로 노출하려면: 인증/네트워크 접근 제어, 세션 격리, 업로드 파일 처리 정책(임시파일 정리), 환자정보 보호 규정 준수를 별도로 검토.
- 업로드된 CSV는 세션의 임시 경로에서만 읽고 앱이 별도로 저장하지 않는다. 환자정보 취급 시 기관 규정을 따를 것.
