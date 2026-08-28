# =====================================================================
# app.R  -  Canonical entry point for the Vancomycin TDM Shiny app
# ---------------------------------------------------------------------
# Standard Shiny directory-app layout: launch with
#     shiny::runApp("TDM-Vanco")   # from the repository root
# which sets the working directory here so the engine/model files are
# always found.  Equivalent to Start4.R (AMC + Inje, model-selectable).
# =====================================================================

# working directory is the app folder under runApp(<dir>); fall back robustly
.tdm_root <- local({
  here  <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA_character_)
  cands <- c(getwd(), if (!is.na(here)) here)
  need  <- c("TDMLIB3.R", "models.R", "tdmApp.R")
  hit   <- cands[vapply(cands, function(d) all(file.exists(file.path(d, need))), logical(1))]
  if (!length(hit))
    stop("Cannot locate TDMLIB3.R / models.R / tdmApp.R next to app.R.")
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
