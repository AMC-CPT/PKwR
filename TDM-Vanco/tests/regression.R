# =====================================================================
# tests/regression.R  -  golden-output regression lock for the TDM engine
# ---------------------------------------------------------------------
# Locks the numerical outputs (EBE, prediction bands, AUC-guided dose) so
# that future edits to TDMLIB3.R / models.R can be proven not to change any
# validated number.  Uses ONLY the synthetic fixtures in tests/fixtures/
# (no real patient data).
#
#   Rscript tests/regression.R make    # (re)create the baseline .rds
#   Rscript tests/regression.R check   # compare current outputs, non-zero exit on any diff
#   Rscript tests/regression.R         # same as "check"
# =====================================================================
options(digits = 15)
mode <- {a <- commandArgs(trailingOnly = TRUE); if (length(a)) a[1] else "check"}

# -- locate repo root (this file lives in <root>/tests) ---------------------
root <- local({
  cands <- c(".", "..")
  hit <- cands[file.exists(file.path(cands, "TDMLIB3.R")) &
               file.exists(file.path(cands, "models.R"))]
  if (!length(hit)) stop("Cannot find TDMLIB3.R + models.R from ", getwd())
  normalizePath(hit[1])
})
source(file.path(root, "TDMLIB3.R"))
source(file.path(root, "models.R"))

FIXDIR   <- file.path(root, "tests", "fixtures")
BASELINE <- file.path(root, "tests", "golden_baseline.rds")

compute <- function() {
  files <- sort(list.files(FIXDIR, pattern = "\\.csv$", full.names = TRUE))
  out <- list()
  for (f in files) {
    D <- prepTDM(f)
    per <- list()
    for (mn in names(PARSETS)) {
      m   <- PARSETS[[mn]]
      ebe <- EBE(PredVanco, D, m$TH, m$OM, m$SG)
      cc  <- D$CLCR[is.finite(D$CLCR)]; clcr <- min(cc[length(cc)], 150)
      tau <- lastTau(D, 12)
      a   <- aucDose(ebe$EBEi, m$TH, clcr, tau, AUC_TARGET, TINF)
      PI  <- calcTDM(PredVanco, D, m$TH, m$SG, ebe,
                     ceiling(max(D$TIME)), a$Dose, a$Dose, a$tau, 5)
      per[[mn]] <- list(EBEi = ebe$EBEi, SE = ebe$SE, COV = ebe$COV, IPRED = ebe$IPRED,
                        auc = a, PI = PI)
    }
    out[[basename(f)]] <- per
  }
  out
}

cur <- compute()

if (identical(mode, "make")) {
  saveRDS(cur, BASELINE)
  cat("Baseline written:", BASELINE, "(", length(cur), "fixtures x", length(PARSETS), "models )\n")
  quit(status = 0)
}

if (!file.exists(BASELINE)) stop("No baseline found; run: Rscript tests/regression.R make")
base <- readRDS(BASELINE)

maxDelta <- 0; diffs <- character(0)
walk <- function(a, b, path) {
  if (is.data.frame(a) && is.data.frame(b)) { for (c in names(a)) walk(a[[c]], b[[c]], paste0(path, "$", c)); return() }
  if (is.list(a) && is.list(b)) { for (k in union(names(a), names(b))) walk(a[[k]], b[[k]], paste0(path, "/", k)); return() }
  if (is.numeric(a) && is.numeric(b) && length(a) == length(b)) {
    d <- suppressWarnings(max(abs(a - b), na.rm = TRUE)); if (!is.finite(d)) d <- 0
    maxDelta <<- max(maxDelta, d)
    if (d > 0) diffs <<- c(diffs, sprintf("%s  max|delta|=%.3e", path, d))
  } else if (!identical(a, b)) diffs <<- c(diffs, paste0(path, "  structural change"))
}
walk(cur, base, "")

if (length(diffs) == 0) {
  cat("REGRESSION PASS: outputs bit-identical to baseline (max|delta|=0).\n")
  quit(status = 0)
} else {
  cat("REGRESSION FAIL: outputs changed vs baseline (max|delta|=", maxDelta, ")\n", sep = "")
  cat(paste0("  ", diffs, collapse = "\n"), "\n")
  quit(status = 1)
}
