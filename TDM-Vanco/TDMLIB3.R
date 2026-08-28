# =====================================================================
# TDMLIB3.R  -  Vancomycin TDM engine (robust, consolidated)
# ---------------------------------------------------------------------
# Supersedes TDMLIB.R and TDMLIB2.R.  Same numerical core (2-compartment
# infusion model + empirical-Bayes estimation), but:
#   1. Robust CSV ingestion  : prepTDM() tolerates the format variants
#      that used to crash the app (DATE vs DAT2 vs numeric TIME, missing
#      optional covariate columns, missing II/ADDL, "." / "" / "NA").
#   2. Correctness fix        : EBE() now inserts infusion-stop change
#      points (expand2) BEFORE calling the vectorised PredVanco, exactly
#      as calcPI() does.  Previously EBE fed raw records to PredVanco,
#      which assumes RATE is constant over each whole inter-record
#      interval -> concentrations ~40x too high -> nonsensical ETAs
#      (and occasional total failure, ETA = 0,0,0,0).
#   3. Plot fix               : calcPI() carries RATE/CLCR forward (LOCF)
#      onto the prediction grid instead of zeroing them, so the infusion
#      is delivered for its full duration.
#   4. Clear validation errors instead of "undefined columns selected".
#
# Public API (unchanged names, drop-in for the Start*.R apps):
#   prepTDM(path)                     -> canonical event data.frame
#   EBE(PRED, DATAi, TH, OM, SG)      -> list(EBEi, SE, COV, IPRED, ...)
#   calcTDM(PRED, DATAi, TH, SG, rEBE, TIME, AMT, RATE, II, ADDL)
#   plotTDM(...) / calcPI(...) / plotPI(...)
#   PredVanco                         -> the forward model
# Back-compat shims: convDT(), expandDATA() still exported.
# =====================================================================

# The empirical-Bayes estimator (EBE) builds its objective as a LOCAL closure,
# so there is no shared mutable scratch environment (each call is self-contained
# and re-entrant -- safe under future async/parallel fitting).

# ---------------------------------------------------------------------
# 1. DATA INGESTION
# ---------------------------------------------------------------------

# Columns the forward model / estimator actually need.
.REQ_COLS  = c("ID", "TIME", "AMT", "RATE", "DV")          # MDV, CLCR derived if absent
.NUM_COLS  = c("TIME", "AMT", "RATE", "DV", "MDV", "II", "ADDL", "EVID",
               "AGE", "SEX", "BWT", "SCR", "ALB", "CLCR")
.ENGINE_COLS = c("ID", "TIME", "AMT", "RATE", "DV", "MDV") # + covariates appended

.parseDateTime = function(dvec, tvec)
{
  s = trimws(paste(dvec, tvec))
  # Dates MUST be ISO year-first: YYYY-MM-DD, with an HH:MM(:SS) clock TIME.
  # Day/month-first formats (DD/MM/YYYY, MM/DD/YYYY, DD-Mon-YYYY, ...) are NOT
  # accepted: they are mutually ambiguous and can silently mis-order a patient's
  # timeline (e.g. "03/04" could be Apr-3 or Mar-4).  Use YYYY-MM-DD only.
  fmts = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")
  for (f in fmts) {
    p = as.POSIXct(s, format = f, tz = "UTC")   # UTC avoids DST discontinuities
    if (!any(is.na(p))) return(p)
  }
  bad = s[is.na(as.POSIXct(s, format = fmts[1], tz = "UTC")) &
          is.na(as.POSIXct(s, format = fmts[2], tz = "UTC"))]
  stop("DATE must be ISO format YYYY-MM-DD (e.g. 2021-11-13) with an HH:MM clock TIME. ",
       "Day/month-first or other date formats are not accepted. Offending value: '",
       if (length(bad)) bad[1] else s[1], "'")
}

# synonyms accepted for the canonical covariate names the model/PK-report use.
# CCR/CCR2 = creatinine clearance (per TDM.CTL: CLCR = CCR2); WT = body weight.
.ALIAS = list(
  CLCR = c("CRCL", "CCR2", "CCR", "CCL", "CLCREAT", "CRCLCG"),
  BWT  = c("WT", "WGT", "WEIGHT", "BW", "WTKG", "BODYWEIGHT"),
  SCR  = c("CREAT", "CREATININE", "SCREAT"))

# find the header row so leading junk (e.g. a NONMEM FDATA "INPUT FILE:" banner
# or a blank line) is skipped; returns lines-to-skip.
.headerSkip = function(path)
{
  L = readLines(path, warn = FALSE, n = 50)
  for (i in seq_along(L)) {
    toks = toupper(trimws(strsplit(L[i], ",")[[1]]))
    if ("ID" %in% toks && "TIME" %in% toks) return(i - 1)
  }
  0
}

# readTDM(): read + normalise columns + resolve TIME to numeric hours.
readTDM = function(x)
{
  D = if (is.data.frame(x)) x else
      read.csv(x, na.strings = c("", ".", "NA", "na", "N/A", "NaN"),
               as.is = TRUE, strip.white = TRUE, check.names = FALSE,
               skip = .headerSkip(x))
  names(D) = toupper(trimws(names(D)))

  # -- resolve covariate synonyms to canonical names (only when the canonical
  #    name is not already present, so a real CLCR column always wins)
  for (canon in names(.ALIAS)) {
    if (!canon %in% names(D)) {
      hit = intersect(.ALIAS[[canon]], names(D))
      if (length(hit)) names(D)[names(D) == hit[1]] = canon
    }
  }

  # -- locate a date column (DATE, DAT2, DATE1, ... or anything starting "DAT")
  dateCol = intersect(c("DATE", "DAT2", "DATE1", "DT", "SDATE"), names(D))
  if (length(dateCol) == 0) {
    cand = grep("^DAT", names(D), value = TRUE)
    dateCol = if (length(cand)) cand[1] else character(0)
  }

  if (!"TIME" %in% names(D)) stop("Required column 'TIME' is missing.")

  # -- decide how to interpret TIME
  timeChr    = as.character(D$TIME)
  timeIsClock = any(grepl(":", timeChr))          # "08:30" style
  if (timeIsClock) {
    if (length(dateCol) == 0)
      stop("TIME looks like clock time (contains ':') but no DATE column was found.")
    sdt      = .parseDateTime(D[[dateCol[1]]], D$TIME)
    D$SDT    = format(sdt, "%Y-%m-%d %H:%M:%S")
    D$TIME   = as.numeric(difftime(sdt, sdt[1], units = "hours"))
  } else {
    D$TIME = suppressWarnings(as.numeric(timeChr))
    if (any(is.na(D$TIME))) stop("TIME column is non-numeric and not clock time.")
  }
  if (length(dateCol)) D[[dateCol[1]]] = NULL     # drop the raw date string

  # -- numeric coercion for known numeric fields that are present
  for (col in intersect(.NUM_COLS, names(D)))
    D[[col]] = suppressWarnings(as.numeric(D[[col]]))

  # -- required event columns
  miss = setdiff(.REQ_COLS, names(D))
  if (length(miss))
    stop("Missing required column(s): ", paste(miss, collapse = ", "),
         ".  Found: ", paste(names(D), collapse = ", "))

  # -- this is a single-patient tool: the timeline is referenced to the first
  #    record (see TIME resolution above) and nothing groups by ID, so a file
  #    with several IDs would be silently fit as one interleaved subject.
  uid = unique(D$ID[!is.na(D$ID)])
  if (length(uid) > 1)
    stop("This tool fits ONE patient at a time, but the file contains ",
         length(uid), " IDs: ", paste(utils::head(uid, 10), collapse = ", "),
         if (length(uid) > 10) ", ..." else "", ".  Please upload a single patient.")

  # -- MDV default (dose/missing = 1, observation = 0)
  if (!"MDV" %in% names(D)) D$MDV = as.integer(is.na(D$DV))

  # -- a DV is an observation ONLY on a measurement row (MDV==0 / EVID==0).
  #    NONMEM FDATA dumps write DV=0 (not ".") on dose rows, which would
  #    otherwise be mistaken for a 0 mg/L observation and corrupt the fit.
  isEvent = (!is.na(D$MDV) & D$MDV != 0)
  if ("EVID" %in% names(D)) isEvent = isEvent | (!is.na(D$EVID) & D$EVID != 0)
  D$DV[isEvent] = NA

  # -- dose rows must carry BOTH AMT>0 and RATE>0.  The forward model delivers
  #    drug purely as RATE integrated over the infusion window (which expand2
  #    derives from AMT/RATE); expandADDL/expand2 also zero BOTH fields when
  #    EITHER is NA.  So a dose with a missing/zero RATE (or a rate with no
  #    amount) would be silently dropped, biasing the fit and every downstream
  #    recommendation.  Fail loud rather than under-dose without warning.
  if ("RATE" %in% names(D)) {
    hasA = !is.na(D$AMT)  & D$AMT  > 0
    hasR = !is.na(D$RATE) & D$RATE > 0
    bad  = which(hasA != hasR)
    if (length(bad))
      stop("Dose record(s) at TIME = ", paste(round(D$TIME[bad], 3), collapse = ", "),
           " have an amount without a matching infusion rate (or vice-versa). ",
           "Every dose must have both AMT > 0 and RATE > 0.")
  }

  # -- CLCR is the only covariate the model uses; compute if absent & possible
  if (!"CLCR" %in% names(D)) {
    if (all(c("AGE", "BWT", "SCR") %in% names(D))) {
      # SEX coding is EXPLICIT: 0 = female, 1 = male.  Cockcroft-Gault applies a
      # 0.85 multiplier for females.
      if ("SEX" %in% names(D)) {
        sx  = D$SEX
        bad = unique(sx[!is.na(sx) & !(sx %in% c(0, 1))])
        if (length(bad))
          stop("SEX must be coded 0 = female or 1 = male; found: ", paste(bad, collapse = ", "), ".")
        sexAdj  = ifelse(sx == 0, 0.85, 1)
        sexNote = "SEX 0=female / 1=male"
      } else {
        sexAdj  = 1
        sexNote = "SEX absent -> assumed male (no 0.85 factor); provide SEX (0=female, 1=male) for accuracy"
      }
      D$CLCR = (140 - D$AGE) * D$BWT / (72 * D$SCR) * sexAdj
      warning("CLCR was absent; computed via Cockcroft-Gault (", sexNote, ").")
    } else {
      stop("Model needs CLCR (or AGE, BWT, SCR to derive it), but none were found.")
    }
  }

  # -- a present-but-partially-blank CLCR would NA-poison the fit (pmin(NA,150)
  #    propagates through PredVanco).  Carry the last measured CLcr forward (and
  #    back-fill any leading gap), mirroring how calcPI() already LOCF-fills CLCR
  #    onto the prediction grid.  A file with a complete CLCR column is untouched.
  if (any(is.na(D$CLCR))) {
    D$CLCR = .locf(D$CLCR, backfillLead = TRUE)
    if (any(is.na(D$CLCR)))
      stop("CLCR is missing for every record and cannot be derived.")
    warning("Some CLCR values were missing; carried forward from the nearest measured value.")
  }
  return(D)
}

# prepTDM(): full ingestion -> canonical, ADDL-expanded event data.
# This is the single entry point the Shiny app should call.
prepTDM = function(x)
{
  D = readTDM(x)
  D = expandADDL(D)
  # order: ID, TIME, AMT, RATE, DV, MDV, then any covariates present
  cov = setdiff(names(D), .ENGINE_COLS)
  D[, c(.ENGINE_COLS, cov)]
}

# ---- back-compat shim: old apps call convDT(read.csv(...)) --------------
# Accepts a data.frame that still has a DATE/DAT2 column and returns the
# same thing readTDM() would (numeric TIME, date dropped).
convDT = function(DATAi) readTDM(DATAi)

# ---------------------------------------------------------------------
# 2. ADDL EXPANSION  (repeat doses)   -- robust replacement of expandDATA
# ---------------------------------------------------------------------
expandADDL = function(DATAo)
{
  # If there is no ADDL column there is nothing to expand.
  if (!"ADDL" %in% names(DATAo) || !"II" %in% names(DATAo)) {
    keep = setdiff(names(DATAo), c("II", "ADDL", "EVID", "SDT"))
    out  = DATAo[, keep, drop = FALSE]
    out[is.na(out$AMT) | is.na(out$RATE), c("AMT", "RATE")] = 0
    return(out)
  }

  rows  = list()
  added = logical(0)
  for (i in seq_len(nrow(DATAo))) {
    rows[[length(rows) + 1]] = DATAo[i, ]
    added = c(added, FALSE)
    if (!is.na(DATAo[i, "ADDL"]) && !is.na(DATAo[i, "II"]) && DATAo[i, "ADDL"] > 0) {
      for (j in seq_len(DATAo[i, "ADDL"])) {
        r = DATAo[i, ]
        r[, "TIME"] = DATAo[i, "TIME"] + j * DATAo[i, "II"]
        rows[[length(rows) + 1]] = r
        added = c(added, TRUE)
      }
    }
  }
  eD = do.call(rbind, rows)

  ord   = order(eD$TIME)
  eD    = eD[ord, ]
  added = added[ord]

  # carry covariates back onto ADDL-generated rows from the next real row
  CovCol = setdiff(names(eD), c("ID", "TIME", "AMT", "RATE", "II", "ADDL",
                                "DV", "MDV", "EVID", "SDT"))
  if (nrow(eD) >= 2) for (i in nrow(eD):2)
    if (added[i - 1]) eD[i - 1, CovCol] = eD[i, CovCol]

  eD[is.na(eD$AMT) | is.na(eD$RATE), c("AMT", "RATE")] = 0
  keep = setdiff(names(eD), c("II", "ADDL", "EVID", "SDT"))
  eD[, keep, drop = FALSE]
}

# back-compat alias
expandDATA = expandADDL

# ---------------------------------------------------------------------
# 3. INFUSION-STOP CHANGE POINTS  (required by the vectorised PredVanco)
# ---------------------------------------------------------------------
# For each infusion (RATE>0) insert a record at TIME + AMT/RATE where the
# infusion stops.  The vectorised model treats RATE as constant over each
# whole [t_{i-1}, t_i] interval, so these stop points are mandatory for a
# correct concentration-time profile.
expand2 = function(DATAo)
{
  DATAo[is.na(DATAo$AMT) | is.na(DATAo$RATE), c("AMT", "RATE")] = 0
  CovCol = setdiff(names(DATAo), c("ID", "TIME", "AMT", "RATE", "II", "ADDL",
                                   "DV", "MDV", "EVID", "SDT"))
  rows = list()
  n    = nrow(DATAo)
  for (i in seq_len(n)) {
    cRow = DATAo[i, ]
    rows[[length(rows) + 1]] = cRow
    rate = cRow[["RATE"]]
    if (!is.na(rate) && rate > 0) {
      sRow = cRow
      sRow[["TIME"]] = cRow[["TIME"]] + cRow[["AMT"]] / rate
      sRow[c("AMT", "RATE")] = 0
      sRow[["DV"]]  = NA          # a stop point is NOT an observation
      sRow[["MDV"]] = 1
      # inherit covariates in effect after this point (next real row, else self)
      if (i < n) sRow[CovCol] = DATAo[i + 1, CovCol]
      rows[[length(rows) + 1]] = sRow
    }
  }
  eD = do.call(rbind, rows)
  eD$TIME = round(eD$TIME, 4)
  eD = eD[order(eD$TIME), ]
  eD[, c("ID", "TIME", "AMT", "RATE", "DV", "MDV", CovCol)]
}

# ---------------------------------------------------------------------
# 4. FORWARD MODEL  (2-compartment infusion, vectorised)
#    NB. requires expand2()-style input (stop points present).
# ---------------------------------------------------------------------
PredVanco = function(TH, ETA, DATAi)
{
  nRec = nrow(DATAi)
  X1 = numeric(nRec); X2 = numeric(nRec)
  CLCR = pmin(DATAi[, "CLCR"], 150)

  CL = TH[1] * CLCR / 100 * exp(ETA[1])
  V1 = TH[2] * exp(ETA[2])
  V2 = TH[3] * exp(ETA[3])
  Q  = TH[4] * exp(ETA[4])
  Ke  = CL / V1
  Kcp = rep(Q / V1, nRec)
  Kpc = rep(Q / V2, nRec)
  L1pL2 = Ke + Kcp + Kpc
  Det4  = sqrt(L1pL2 * L1pL2 - 4 * Ke * Kpc)
  L1 = (L1pL2 + Det4) / 2
  L2 = (L1pL2 - Det4) / 2

  dTime = c(0, diff(DATAi[, "TIME"]))
  E1 = exp(-L1 * dTime); E2 = exp(-L2 * dTime)
  C1 = L1 - Kpc; C2 = Kpc - L2
  m11 = C1 * E1 + C2 * E2
  m12 = -Kpc * E1 + Kpc * E2
  m21 = -Kcp * E1 + Kcp * E2
  m22 = (L1 - Ke - Kcp) * E1 + (Ke + Kcp - L2) * E2
  E1m = 1 - E1; E2m = 1 - E2

  R = DATAi[, "RATE"]
  if (nRec >= 2) for (i in 2:nRec) {
    X1[i] = (m11[i]*X1[i-1] + m12[i]*X2[i-1] + R[i-1]*(C1[i]*E1m[i]/L1[i] + C2[i]*E2m[i]/L2[i])) / (L1[i] - L2[i])
    X2[i] = (m21[i]*X1[i-1] + m22[i]*X2[i-1] + R[i-1]*(-Kcp[i]*E1m[i]/L1[i] + Kcp[i]*E2m[i]/L2[i])) / (L1[i] - L2[i])
  }
  X1 / V1
}

# ---------------------------------------------------------------------
# 5. NUMERICAL GRADIENT (Richardson extrapolation), used for SE/PI bands
# ---------------------------------------------------------------------
mGrad = function(func, x, nRec)
{
  n  = length(x)
  x1 = x; x2 = x
  ga = matrix(nrow = nRec, ncol = 4)
  gr = matrix(nrow = nRec, ncol = n)
  for (i in 1:n) {
    hi = if (abs(x[i]) <= 1) 1e-4 else 1e-4 * abs(x[i])
    for (k in 1:4) {
      x1[i] = x[i] - hi; x2[i] = x[i] + hi
      ga[, k] = (func(x2) - func(x1)) / (2 * hi)
      hi = hi / 2
    }
    ga[, 1] = (ga[, 2]*4  - ga[, 1]) / 3
    ga[, 2] = (ga[, 3]*4  - ga[, 2]) / 3
    ga[, 3] = (ga[, 4]*4  - ga[, 3]) / 3
    ga[, 1] = (ga[, 2]*16 - ga[, 1]) / 15
    ga[, 2] = (ga[, 3]*16 - ga[, 2]) / 15
    gr[, i] = (ga[, 2]*64 - ga[, 1]) / 63
    x1[i] = x2[i] = x[i]
  }
  gr
}

# ---------------------------------------------------------------------
# 6. EMPIRICAL BAYES ESTIMATION
# ---------------------------------------------------------------------
# EBE(): fit ETA, then return predictions/SE/SD on the SAME expand2()-ed
# data (returned as $DATAe) so the caller can align IPRED row-for-row.
EBE = function(PRED, DATAi, TH, OM, SG)
{
  DATAe = expand2(DATAi)                 # <-- the fix: stop points before prediction
  if (!any(!is.na(DATAe$DV)))
    stop("No observations (DV) found - cannot estimate individual parameters.")

  # An ETA with no inter-individual variability (Omega diagonal == 0) makes
  # Omega singular.  Regularize those to a tiny value so Omega is invertible;
  # the huge resulting prior precision pins that ETA at ~0 (i.e. fixed effect).
  omd = diag(OM); if (any(omd <= 0)) diag(OM)[omd <= 0] = 1e-6

  invOM = solve(OM); obs = !is.na(DATAe$DV); nEta = nrow(OM)

  # Objective as a LOCAL closure: it captures PRED / DATAe / TH / SG / invOM /
  # obs by lexical scope, so each EBE() call is self-contained and re-entrant
  # (no shared mutable scratch env that concurrent/async fits could clobber).
  ObjEta = function(ETAi)
  {
    Fi = PRED(TH, ETAi, DATAe)[obs]
    Ri = DATAe[obs, "DV"] - Fi
    Hi = cbind(Fi, 1)
    Vi = diag(Hi %*% SG %*% t(Hi))
    # Drop observations whose residual variance is non-positive.  With a
    # proportional-only error model (e.g. AMC, no additive term) a sample at or
    # before the first dose predicts exactly 0 for every ETA, so Vi = 0 and the
    # term log(0)+Ri^2/0 = NaN used to abort optim() on the (default) AMC model.
    # Such a point is ETA-independent (fixed prediction 0, zero gradient), so it
    # carries no information and excluding it leaves the estimate unchanged while
    # keeping the objective finite.  For obs with Vi>0 the mask is all-TRUE, so
    # this is bit-identical to the previous objective (Inje always has Vi>0).
    ok = is.finite(Vi) & Vi > 0
    if (!any(ok)) return(as.numeric(t(ETAi) %*% invOM %*% ETAi))   # prior-only fallback
    sum(log(Vi[ok]) + Ri[ok] * Ri[ok] / Vi[ok]) + as.numeric(t(ETAi) %*% invOM %*% ETAi)
  }

  # Multi-start MAP estimation.  With sparse TDM data the objective can be flat
  # near the origin (BFGS from 0 stalls) or bi-modal (V2/ETA3), so we search from
  # the origin plus +/-1.5 prior-SD along each ETA axis, Nelder-Mead then BFGS at
  # each, and keep the global best.  Verified to reproduce the reference NONMEM
  # EBEs (EBEALL-from BAIK1-FINAL2b) for all 56 BAIK1 patients.
  priorSD = sqrt(diag(OM))
  starts  = list(rep(0, nEta))
  for (k in 1:nEta) for (sg in c(-1.5, 1.5)) {
    v = rep(0, nEta); v[k] = sg * priorSD[k]; starts = c(starts, list(v))
  }
  best = rep(0, nEta); bestVal = Inf
  for (st in starts) {
    rn = optim(st,      ObjEta, method = "Nelder-Mead", control = list(maxit = 500))
    rb = optim(rn$par,  ObjEta, method = "BFGS")
    if (rb$value < bestVal) { bestVal = rb$value; best = rb$par }
  }
  r0   = optim(best, ObjEta, method = "BFGS")   # final polish of the global-best mode
  if (r0$value > bestVal + 1e-8) r0$par = best
  EBEi = r0$par
  Fi   = PRED(TH, EBEi, DATAe)
  Ri   = DATAe[obs, "DV"] - Fi[obs]
  nRec = length(Fi)

  # Conditional (posterior) covariance via the Gauss-Newton / FOCE information
  # matrix:  COV = [ G' Sigma^-1 G + Omega^-1 ]^-1,  where G = df/deta at the
  # observations and Sigma = diag(residual variance).  This is always positive
  # definite, so every SE is finite and bounded by the prior SD -- unlike
  # inverting the full numerical Hessian (COV = 2 H^-1), which on sparse
  # individual data can turn non-positive-definite and return an SE larger than
  # the prior, which is impossible for a true posterior.
  PREDij = function(ETA) PRED(TH, ETA, DATAe)
  gr1  = mGrad(PREDij, EBEi, nRec)                 # (nRec x nEta) prediction gradients
  Gobs = gr1[obs, , drop = FALSE]                # gradients at the observations
  Vobs = Fi[obs]^2 * SG[1, 1] + 2 * Fi[obs] * SG[1, 2] + SG[2, 2]   # residual variance,
  #      matching ObjEta's Vi (the cross term is 0 for the diagonal SIGMAs shipped here)
  oko  = is.finite(Vobs) & Vobs > 0                # same zero-variance drop as ObjEta (all-TRUE, hence
  Gobs = Gobs[oko, , drop = FALSE]; Vobs = Vobs[oko]  #   bit-identical, unless a zero-prediction obs exists)
  COV  = tryCatch(solve(t(Gobs) %*% (Gobs / Vobs) + invOM), error = function(err) OM)
  SE   = sqrt(pmax(diag(COV), 0))
  illCond = sum(oko) < nEta                      # informative samples vs random effects -> partly prior-driven

  VF  = .nnf(diag(gr1 %*% COV %*% t(gr1)))
  SEy = sqrt(VF)
  SDy = sqrt(.nnf(VF + VF * SG[1, 1] + Fi * Fi * SG[1, 1] + SG[2, 2]))

  Res = list(EBEi, SE, COV, Fi, SEy, SDy, Ri, DATAe, illCond)
  names(Res) = c("EBEi", "SE", "COV", "IPRED", "SE.IPRED", "SD.IPRED", "IRES", "DATAe", "illCond")
  Res
}

# ---------------------------------------------------------------------
# 7. PREDICTION INTERVALS ON A FINE GRID
# ---------------------------------------------------------------------
# DATAi here is already ADDL-expanded event data; we expand2() it (stop
# points), add a dense time grid, and CARRY RATE/CLCR FORWARD onto the
# grid so the infusion is delivered over its true duration.
calcPI = function(PRED, DATAi, TH, SG, rEBE, npoints = 500)
{
  EBEi = rEBE$EBEi
  COV  = rEBE$COV
  De   = expand2(DATAi)

  gridT = seq(0, max(De$TIME), length = npoints)
  gridT = setdiff(round(gridT, 4), De$TIME)          # avoid duplicate-TIME cross join
  D2    = merge(De, data.frame(TIME = gridT), by = "TIME", all = TRUE)
  D2    = D2[order(D2$TIME), ]

  # LOCF for the piecewise-constant infusion state + covariate
  D2$RATE = .locf(D2$RATE)
  D2$AMT[is.na(D2$AMT)] = 0
  if ("CLCR" %in% names(D2)) D2$CLCR = .locf(D2$CLCR, backfillLead = TRUE)

  y2   = PRED(TH, EBEi, D2)
  nR2  = length(y2)
  PREDij = function(ETA) PRED(TH, ETA, D2)
  gr2  = mGrad(PREDij, EBEi, nR2)
  VF2  = .nnf(diag(gr2 %*% COV %*% t(gr2)))
  SEy2 = sqrt(VF2)
  SDy2 = sqrt(.nnf(VF2 + VF2 * SG[1, 1] + y2 * y2 * SG[1, 1] + SG[2, 2]))

  data.frame(x = D2$TIME, y = D2$DV, y2 = y2,
             yciLL = y2 - 1.96 * SEy2, yciUL = y2 + 1.96 * SEy2,
             ypiLL = y2 - 1.96 * SDy2, ypiUL = y2 + 1.96 * SDy2)
}

# non-negative, finite variance (guards sqrt against NaN/negative/Inf)
.nnf = function(v) { v[!is.finite(v)] = 0; pmax(v, 0) }

# last-observation-carried-forward; optionally back-fill any leading NAs
.locf = function(v, backfillLead = FALSE)
{
  if (length(v) >= 2) for (i in 2:length(v)) if (is.na(v[i])) v[i] = v[i - 1]
  if (backfillLead && is.na(v[1]))
    for (i in seq_along(v)) if (!is.na(v[i])) { v[seq_len(i - 1)] = v[i]; break }
  v
}

plotPI = function(PRED, DATAi, TH, SG, rEBE, npoints = 500)
{
  Res = calcPI(PRED, DATAi, TH, SG, rEBE, npoints)
  dev.new()
  with(Res, {
    yl = range(c(ypiLL, ypiUL, y), na.rm = TRUE); if (!all(is.finite(yl))) yl = c(0, 40)
    plot(0, 0, type = "n", xlab = "Time", ylab = "Concentration +/- 2SD",
         xlim = range(x, na.rm = TRUE), ylim = yl)
    points(x[!is.na(y)], y[!is.na(y)], pch = 16)
    lines(x, y2, lty = 1)
    lines(x, yciLL, lty = 2, col = "red");  lines(x, yciUL, lty = 2, col = "red")
    lines(x, ypiLL, lty = 3, col = "blue"); lines(x, ypiUL, lty = 3, col = "blue")
    abline(h = c(5, 15, 25, 35), lty = 2)
  })
  Res
}

# ---------------------------------------------------------------------
# 8. FUTURE-DOSE SIMULATION (TDM recommendation)
# ---------------------------------------------------------------------
# Append a proposed future regimen (single dose or II/ADDL series) to the
# patient's history, then compute prediction intervals.
addDATAi = function(DATAi, TIME, AMT, RATE, II, ADDL)
{
  lRow = DATAi[nrow(DATAi), ]
  for (i in seq_len(ADDL + 1)) {
    aRow = lRow
    aRow[, "TIME"] = TIME + (i - 1) * II
    aRow[, "AMT"]  = AMT
    aRow[, "RATE"] = RATE
    aRow[, "DV"]   = NA
    aRow[, "MDV"]  = 1
    DATAi = rbind(DATAi, aRow)
  }
  DATAi
}

calcTDM = function(PRED, DATAi, TH, SG, rEBE, TIME, AMT, RATE, II, ADDL, npoints = 500)
{
  DATAi = addDATAi(DATAi, TIME, AMT, RATE, II, ADDL)
  calcPI(PRED, DATAi, TH, SG, rEBE, npoints)   # calcPI does the expand2 + grid
}

plotTDM = function(PRED, DATAi, TH, SG, rEBE, TIME, AMT, RATE, II, ADDL, npoints = 500)
{
  DATAi = addDATAi(DATAi, TIME, AMT, RATE, II, ADDL)
  plotPI(PRED, DATAi, TH, SG, rEBE, npoints)
}

# ---------------------------------------------------------------------
# 9. AUC-GUIDED DOSING
# ---------------------------------------------------------------------
# lastTau(): the last real inter-dose interval, snapped to the nearest standard
# dosing interval (default 8/12/24/36/48 h).  Dose records < 2 h apart are
# collapsed (a level drawn at the same time as a dose is a data artifact, not an
# interval).  Uses `fallback` when there is no usable interval.
lastTau = function(DATAi, fallback = 12, snap = c(8, 12, 24, 36, 48))
{
  dt = sort(unique(DATAi[!is.na(DATAi$AMT) & DATAi$AMT > 0, "TIME"]))
  if (length(dt) >= 2) dt = dt[c(TRUE, diff(dt) >= 2)]
  tau = if (length(dt) >= 2) dt[length(dt)] - dt[length(dt) - 1] else NA
  if (is.na(tau) || tau <= 0) tau = fallback
  if (length(snap)) tau = snap[which.min(abs(snap - tau))]   # nearest standard interval
  tau
}

# indivPK(): individual PK parameters + micro-constants from the ETAs.
# CL is CLcr-scaled exactly as in PredVanco (CL = TH[1]*CLcr/100*exp(ETA1)).
indivPK = function(eta, TH, clcrLast)
{
  clcrLast = min(clcrLast, 150)                    # same cap PredVanco applies (pmin(CLCR,150))
  CL = TH[1] * exp(eta[1]) * clcrLast / 100
  V1 = TH[2] * exp(eta[2]); V2 = TH[3] * exp(eta[3]); Q = TH[4] * exp(eta[4])
  list(CL = CL, V1 = V1, V2 = V2, Q = Q, k10 = CL / V1, k12 = Q / V1, k21 = Q / V2)
}

# ssMetrics(): steady-state daily AUC and Cmin/Cmax for a GIVEN regimen
# (2-compartment infusion).  pk from indivPK(); Dose (mg); tau interval (h);
# Tinf infusion duration (h).  Daily AUC = Dose/CL*24/tau (Tinf-independent);
# Cmin/Cmax depend on Tinf.
ssMetrics = function(pk, Dose, tau, Tinf)
{
  V1 = pk$V1; k10 = pk$k10; k12 = pk$k12; k21 = pk$k21
  S    = k10 + k12 + k21
  beta = (S - sqrt(S^2 - 4 * k21 * k10)) / 2
  alph = k21 * k10 / beta
  Ax = (alph - k21) / (alph - beta) / V1 / alph
  Bx = (beta - k21) / (beta - alph) / V1 / beta
  eAlpT = exp(alph * tau); eAlpD = exp(alph * Tinf)
  eBetT = exp(beta * tau); eBetD = exp(beta * Tinf)
  CminSS = Dose / Tinf * (Ax * (eAlpD - 1) / (eAlpT - 1) + Bx * (eBetD - 1) / (eBetT - 1))
  CmaxSS = Dose / Tinf * (Ax * (1 - 1/eAlpD) / (1 - 1/eAlpT) + Bx * (1 - 1/eBetD) / (1 - 1/eBetT))
  list(AUC = Dose / pk$CL * 24 / tau, CminSS = CminSS, CmaxSS = CmaxSS)
}

# aucDose(): single dose that hits a target daily AUC (rounded to nearest 10 mg),
# plus the steady-state metrics it produces.  Returns pk fields + tau, Dose,
# AUC (delivered daily AUC of the rounded dose), CminSS, CmaxSS.
aucDose = function(eta, TH, clcrLast, tau, AUCtarget = 410, Tinf = 1)
{
  pk   = indivPK(eta, TH, clcrLast)
  Dose = floor(AUCtarget * pk$CL * tau / 24 / 10 + 0.5) * 10    # nearest 10 mg
  ss   = ssMetrics(pk, Dose, tau, Tinf)
  c(pk, list(tau = tau, Dose = Dose, AUC = ss$AUC, CminSS = ss$CminSS, CmaxSS = ss$CmaxSS))
}
