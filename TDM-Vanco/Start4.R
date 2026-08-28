# =====================================================================
# Start4.R  -  Vancomycin TDM, AUC-guided dosing (model-selectable: AMC/Inje)
# ---------------------------------------------------------------------
# Thin wrapper.  The real app lives in tdmApp.R; the models in models.R;
# the numerical engine in TDMLIB3.R.  (Start3.R is the single-model Inje
# variant of this same factory.)
# =====================================================================

# --- locate the app folder, then load engine + models + factory -----------
.tdm_root <- local({
  here  <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA_character_)
  cands <- c(if (!is.na(here)) here, getwd())
  need  <- c("TDMLIB3.R", "models.R", "tdmApp.R")
  hit   <- cands[vapply(cands, function(d) all(file.exists(file.path(d, need))), logical(1))]
  if (!length(hit))
    stop("Cannot locate TDMLIB3.R / models.R / tdmApp.R. ",
         "Launch from the app folder, e.g. shiny::runApp('TDM-Vanco').")
  hit[1]
})
source(file.path(.tdm_root, "TDMLIB3.R"))
source(file.path(.tdm_root, "models.R"))
source(file.path(.tdm_root, "tdmApp.R"))

tdmApp(models     = PARSETS,
       selected   = "AMC",
       title      = "Vancomycin TDM - AUC-guided dosing",
       auc_target = AUC_TARGET,
       tinf       = TINF,
       help_md    = file.path(.tdm_root, "help_user.md"))
