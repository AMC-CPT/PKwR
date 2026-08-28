# Launch the Vancomycin TDM app.
# Point runApp at the app FOLDER (which contains app.R) so Shiny sets the
# working directory here and the engine/model files are always found.
appDir <- "C:/R/TDM-Vanco"
shiny::runApp(appDir, port = 8080, host = "127.0.0.1")

appDir <- "C:/R/TDM-Vanco"
shiny::runApp(appDir, port = 8080, host = "210.122.172.43")

appDir <- "C:/R/TDM-Vanco"
shiny::runApp(appDir, port = 8080, host = "0.0.0.0")


# --- alternatives: run ONE at a time (runApp blocks until you stop it) -----
# shiny::runApp(file.path(appDir, "Start4.R"), port = 8080, host = "127.0.0.1")  # AMC + Inje
# shiny::runApp(file.path(appDir, "Start3.R"), port = 8080, host = "127.0.0.1")  # Inje only
