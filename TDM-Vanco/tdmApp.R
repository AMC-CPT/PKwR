# =====================================================================
# tdmApp.R  -  The single Vancomycin TDM Shiny app (factory)
# ---------------------------------------------------------------------
# Start3.R and Start4.R used to be ~90% byte-identical.  This factory is
# the one real app; the entry files are thin wrappers that source the
# engine + models and call tdmApp():
#
#   tdmApp(models, selected, title, auc_target, tinf) -> shinyApp object
#
#   models      named list of list(TH, OM, SG); a "Population model" radio
#               is shown only when length(models) > 1.
#   selected    which model is active initially (default: first).
#   title       browser/title-bar text.
#   auc_target  target daily AUC (mg*h/L)   [see models.R].
#   tinf        infusion duration used for the recommended dose (h).
#
# The forward model / estimator (PredVanco, prepTDM, EBE, calcTDM,
# indivPK, ssMetrics, aucDose, lastTau) come from TDMLIB3.R, which the
# entry file sources before calling tdmApp().
# =====================================================================
library(shiny)

# render a Markdown file to HTML for the in-app Help tab (commonmark if present,
# otherwise show the raw text so the manual is never blank).
.tdmRenderHelp <- function(path) {
  if (is.null(path) || !file.exists(path))
    return(tags$p(style = "color:#888;", "help_user.md not found next to the app."))
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (requireNamespace("commonmark", quietly = TRUE))
    return(div(class = "tdm-help", style = "max-width:860px;line-height:1.5;",
               HTML(commonmark::markdown_html(txt, extensions = TRUE))))
  tags$pre(style = "white-space:pre-wrap;", txt)
}

tdmApp <- function(models,
                   selected   = names(models)[1],
                   title      = "Vancomycin TDM",
                   auc_target = 410,
                   tinf       = 1,
                   help_md    = NULL) {

  stopifnot(is.list(models), length(models) >= 1, !is.null(names(models)))
  if (!selected %in% names(models)) selected <- names(models)[1]
  multi <- length(models) > 1

  # data-driven radio labels, e.g. "1. AMC (3 ETAs)" / "2. Inje (4 ETAs)"
  modelChoices <- setNames(
    names(models),
    vapply(seq_along(models), function(i)
      sprintf("%d. %s (%d ETAs)", i, names(models)[i], sum(diag(models[[i]]$OM) > 0)),
      character(1)))

  # ------------------------------------------------------------------- UI
  ui <- fluidPage(
    titlePanel(title),
    sidebarLayout(
      sidebarPanel(
        if (multi)
          radioButtons("paramset", "Population model",
                       choices = modelChoices, selected = selected),
        fileInput("file1", "Upload CSV (NONMEM-style)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        helpText("Needs at least: ID, TIME, AMT, RATE, DV, CLCR. ",
                 "DATE (or DAT2) + clock TIME, or a numeric TIME, are both accepted. ",
                 "Missing values may be '.', '' or 'NA'."),
        tags$hr(),
        strong("Proposed next regimen (for the graph)"),
        numericInput("TIME", "Next dosing time (h)",   value = 0,    min = 0),
        numericInput("AMT",  "Amount (mg)",            value = 1000, min = 0),
        numericInput("RATE", "Rate (mg/h)",            value = 1000, min = 0),
        numericInput("II",   "Interdose interval (h)", value = 12,   min = 0),
        numericInput("ADDL", "Additional doses",       value = 3,    min = 0),
        actionButton("goButton", "Draw Graph", class = "btn-primary"),
        tags$hr(),
        uiOutput("xlim_slider")
      ),
      mainPanel(
        uiOutput("status"),
        tabsetPanel(type = "tabs",
          tabPanel("Data and Graph",
                   plotOutput("plot"),
                   tags$hr(), tableOutput("contents"),
                   tags$hr(),
                   tags$h4(sprintf("AUC-guided recommendation (target daily AUC = %g mg·h/L)", auc_target)),
                   tableOutput("aucRec"),
                   tags$h5("Proposed regimen (Amount / Rate / Interdose interval on the left)"),
                   uiOutput("aucPropNote"),
                   tableOutput("aucProp")),
          tabPanel("Input Data",  tableOutput("DataTable")),
          tabPanel("PK Parameter", tableOutput("PKpara")),
          tabPanel("도움말 (Help)", .tdmRenderHelp(help_md))
        )
      )
    )
  )

  # --------------------------------------------------------------- server
  server <- function(input, output, session) {

    # active model (radio when multi, else the fixed one)
    mdl <- reactive(models[[ if (multi && !is.null(input$paramset)) input$paramset else selected ]])

    # ---- robust ingestion; surfaces a friendly message on any problem ----
    getDATAi <- reactive({
      req(input$file1)
      D <- tryCatch(prepTDM(input$file1$datapath), error = function(err) err)
      if (inherits(D, "error"))
        validate(need(FALSE, paste("Could not read this file:", conditionMessage(D))))
      validate(need(any(!is.na(D$DV)), "No observed concentrations (DV) found in this file."))
      D
    })

    getEBE <- reactive({
      D <- getDATAi(); m <- mdl()
      ebe <- tryCatch(EBE(PredVanco, D, m$TH, m$OM, m$SG), error = function(err) err)
      if (inherits(ebe, "error"))
        validate(need(FALSE, paste("Could not estimate individual parameters:", conditionMessage(ebe))))
      ebe
    })

    # last non-missing, capped CLcr (drives the individual CL used everywhere)
    lastCLcr <- reactive({
      D <- getDATAi()
      cc <- D$CLCR[is.finite(D$CLCR)]
      validate(need(length(cc) > 0, "No usable creatinine clearance (CLCR) in this file."))
      min(cc[length(cc)], 150)
    })

    # individual PK + AUC-guided dose + steady-state Cmin/Cmax
    getAUC <- reactive({
      m <- mdl(); eta <- getEBE()$EBEi
      tau <- lastTau(getDATAi(), 12)          # fixed fallback; independent of the II input field
      aucDose(eta, m$TH, lastCLcr(), tau, auc_target, tinf)
    })

    # ---- regimen to plot ------------------------------------------------
    # On a NEW file (or a model switch before any manual edit) default the
    # inputs to the recommended dose and draw once; on "Draw Graph" use
    # whatever the user entered and remember that they edited it, so a later
    # model switch does not silently discard their regimen.
    rv <- reactiveValues(regimen = NULL, userEdited = FALSE)

    applyRecommendation <- function() {
      a <- getAUC(); D <- getDATAi()
      nextT <- ceiling(max(D$TIME))
      updateNumericInput(session, "TIME", value = nextT)
      updateNumericInput(session, "AMT",  value = a$Dose)
      updateNumericInput(session, "RATE", value = a$Dose)   # RATE = Dose -> Tinf = 1 h
      updateNumericInput(session, "II",   value = a$tau)
      updateNumericInput(session, "ADDL", value = 5)
      rv$regimen    <- list(TIME = nextT, AMT = a$Dose, RATE = a$Dose, II = a$tau, ADDL = 5)
      rv$userEdited <- FALSE
    }

    observeEvent(input$file1, {
      req(input$file1)
      rv$userEdited <- FALSE
      applyRecommendation()
    })

    if (multi) observeEvent(input$paramset, {
      req(input$file1)
      if (!isTRUE(rv$userEdited)) applyRecommendation()   # else keep the user's regimen
    }, ignoreInit = TRUE)

    observeEvent(input$goButton, {
      rv$regimen    <- list(TIME = input$TIME, AMT = input$AMT, RATE = input$RATE,
                            II = input$II, ADDL = input$ADDL)
      rv$userEdited <- TRUE
    }, ignoreInit = TRUE)

    getPI <- reactive({
      D <- getDATAi(); m <- mdl(); rEBE <- getEBE(); reg <- rv$regimen
      req(reg)
      fin <- function(v) is.numeric(v) && length(v) == 1 && is.finite(v)
      validate(need(fin(reg$TIME) && fin(reg$AMT) && fin(reg$RATE) && fin(reg$II) && fin(reg$ADDL),
                    "Enter numeric values for the proposed next regimen (time, amount, rate, interval, additional doses)."))
      validate(need(reg$ADDL == round(reg$ADDL) && reg$ADDL >= 0 && reg$ADDL <= 60,
                    "Additional doses must be a whole number between 0 and 60."))
      validate(need(!(reg$AMT > 0 && reg$RATE <= 0),
                    "Proposed regimen: Rate must be > 0 when Amount > 0."))
      validate(need(reg$ADDL < 1 || reg$RATE <= 0 || reg$AMT / reg$RATE < reg$II,
                    "For repeated doses, the infusion duration (Amount/Rate) must be shorter than the interval."))
      tryCatch(
        calcTDM(PredVanco, D, m$TH, m$SG, rEBE, reg$TIME, reg$AMT, reg$RATE, reg$II, reg$ADDL),
        error = function(err) {
          validate(need(FALSE, paste("Could not simulate the regimen:", conditionMessage(err)))); NULL })
    })

    # ---- status / warning banner ----------------------------------------
    output$status <- renderUI({
      req(input$file1)
      ebe <- getEBE(); obs <- !is.na(ebe$DATAe$DV); msgs <- character(0)
      if (sum(obs) == 1)
        msgs <- c(msgs, "Only one measured concentration: the individual estimate is largely determined by the population prior - interpret the PK parameters and the prediction with caution.")
      rmse <- sqrt(mean((ebe$IPRED[obs] - ebe$DATAe$DV[obs])^2))
      if (is.finite(rmse) && rmse > 8)
        msgs <- c(msgs, sprintf("Large fit residual (RMSE %.1f mg/L): check for outlying or mistimed samples.", rmse))
      if (!length(msgs)) return(NULL)
      div(style = "background:#fff3cd;border:1px solid #ffe69c;padding:8px;border-radius:4px;margin-bottom:8px;",
          lapply(msgs, function(m) div(icon("triangle-exclamation"), " ", m)))
    })

    # ---- tabs -----------------------------------------------------------
    output$DataTable <- renderTable({
      req(input$file1)
      read.csv(input$file1$datapath, na.strings = c("", ".", "NA", "na"),
               as.is = TRUE, check.names = FALSE)
    })

    output$contents <- renderTable({
      ebe <- getEBE(); cbind(ebe$DATAe, IPRED = ebe$IPRED)
    })

    output$aucRec <- renderTable({
      a <- getAUC()
      data.frame(
        Quantity = c("Last dosing interval  τ (h)", "Estimated CL (L/h)",
                     "Recommended single dose (mg)", "Expected daily AUC (mg·h/L)",
                     "Predicted Css,max (mg/L)", "Predicted Css,min (mg/L)"),
        Value = c(sprintf("%.1f", a$tau), sprintf("%.2f", a$CL),
                  sprintf("%.0f", a$Dose), sprintf("%.0f", a$AUC),
                  sprintf("%.1f", a$CmaxSS), sprintf("%.1f", a$CminSS)),
        stringsAsFactors = FALSE)
    }, colnames = FALSE, width = "480px")

    # note when the proposed-regimen metrics (live sidebar values) differ from
    # the regimen actually plotted above (last "Draw Graph")
    output$aucPropNote <- renderUI({
      reg <- rv$regimen; if (is.null(reg)) return(NULL)
      same <- function(a, b) is.numeric(a) && is.finite(a) && isTRUE(a == b)
      pending <- !(same(input$AMT, reg$AMT) && same(input$RATE, reg$RATE) && same(input$II, reg$II))
      if (!pending) return(NULL)
      div(style = "color:#8a6d3b;font-size:90%;margin-bottom:4px;",
          icon("pen"), " These metrics use the sidebar values; the graph above shows the last regimen you drew. ",
          "Click \"Draw Graph\" to plot these.")
    })

    # proposed regimen: recomputed live from Amount / Rate / Interdose interval
    output$aucProp <- renderTable({
      pk <- getAUC()
      amt <- input$AMT; rate <- input$RATE; ii <- input$II
      validate(need(is.numeric(amt) && amt > 0 && is.numeric(rate) && rate > 0 && is.numeric(ii) && ii > 0,
                    "Enter Amount > 0, Rate > 0, and Interdose interval > 0 for the proposed-regimen metrics."))
      Tinf <- amt / rate
      validate(need(Tinf < ii, "Infusion duration (Amount/Rate) must be shorter than the interval."))
      ss <- ssMetrics(pk, amt, ii, Tinf)
      data.frame(
        Quantity = c("Infusion duration Tinf (h)", "Expected daily AUC (mg·h/L)",
                     "Predicted Css,max (mg/L)", "Predicted Css,min (mg/L)"),
        Value = c(sprintf("%.2f", Tinf), sprintf("%.0f", ss$AUC),
                  sprintf("%.1f", ss$CmaxSS), sprintf("%.1f", ss$CminSS)),
        stringsAsFactors = FALSE)
    }, colnames = FALSE, width = "480px")

    output$PKpara <- renderTable({
      eta <- getEBE()$EBEi; pk <- indivPK(eta, mdl()$TH, lastCLcr())
      Res <- data.frame(EBE = eta, Estimate = c(pk$CL, pk$V1, pk$V2, pk$Q),
                        Unit = c("L/h", "L", "L", "L/h"))
      rownames(Res) <- c("CL", "V1", "V2", "Q")
      Res
    }, rownames = TRUE)

    output$xlim_slider <- renderUI({
      PI <- getPI(); req(PI)
      rng <- range(PI$x, na.rm = TRUE)
      if (!all(is.finite(rng))) return(NULL)
      lo <- floor(rng[1]); hi <- ceiling(rng[2]); if (hi <= lo) hi <- lo + 1
      sliderInput("range", "Time range (h)", min = lo, max = hi, value = c(lo, hi))
    })

    output$plot <- renderPlot({
      PI <- getPI(); req(PI)
      rng   <- range(PI$x, na.rm = TRUE)
      xlm   <- if (!is.null(input$range) && all(is.finite(input$range))) input$range else rng
      ypiLL <- pmax(PI$ypiLL, 0); yciLL <- pmax(PI$yciLL, 0)   # concentration >= 0 (display clamp only)
      yl    <- range(c(0, ypiLL, PI$ypiUL, PI$y), na.rm = TRUE)
      if (!all(is.finite(yl))) yl <- c(0, 40)

      plot(0, 0, type = "n", xlab = "Time (h)", ylab = "Concentration +/- 2SD", xlim = xlm, ylim = yl)
      points(PI$x[!is.na(PI$y)], PI$y[!is.na(PI$y)], pch = 16)
      lines(PI$x, PI$y2, lty = 1)
      lines(PI$x, yciLL, lty = 2, col = "red");  lines(PI$x, PI$yciUL, lty = 2, col = "red")
      lines(PI$x, ypiLL, lty = 3, col = "blue"); lines(PI$x, PI$ypiUL, lty = 3, col = "blue")
      abline(h = c(5, 15, 25, 35), lty = 2)
    })
  }

  shinyApp(ui, server)
}
