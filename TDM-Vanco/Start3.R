# =====================================================================
# Start3.R  -  Vancomycin TDM (single model: Inje)
# ---------------------------------------------------------------------
# Thin wrapper.  Same factory as Start4.R but with only the Inje model
# (no model selector), reproducing the original Start3 behaviour.
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

tdmApp(models     = PARSETS["Inje"],
       title      = "Vancomycin TDM",
       auc_target = AUC_TARGET,
       tinf       = TINF,
       help_md    = file.path(.tdm_root, "help_user.md"))
