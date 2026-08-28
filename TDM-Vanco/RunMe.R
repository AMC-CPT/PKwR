# Launch the Vancomycin TDM app.
# Point runApp at the app FOLDER (which contains app.R) so Shiny sets the
# working directory here and the engine/model files are always found.
appDir <- local({
  here  <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                    error = function(e) NA_character_)
  cands <- c(getwd(), if (!is.na(here)) here)
  hit   <- cands[file.exists(file.path(cands, "app.R"))]
  if (!length(hit)) stop("Cannot find app.R. Source RunMe.R from the app folder.")
  hit[1]
})
shiny::runApp(appDir, port = 8080, host = "127.0.0.1")

# Serve on every interface instead (only on a network you trust: the app
# has no authentication and accepts uploaded patient files).
# shiny::runApp(appDir, port = 8080, host = "0.0.0.0")

# --- alternatives: run ONE at a time (runApp blocks until you stop it) -----
# shiny::runApp(file.path(appDir, "Start4.R"), port = 8080, host = "127.0.0.1")  # AMC + Inje
# shiny::runApp(file.path(appDir, "Start3.R"), port = 8080, host = "127.0.0.1")  # Inje only
