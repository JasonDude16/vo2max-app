# app.R
library(shiny)
library(ggplot2)
library(DT)
library(gridExtra)

# ---- Utah Theme ----
UTAH_RED <- "#CC0000"  # Utah Crimson
UTAH_BLACK <- "#000000"
UTAH_GRAY <- "#6b7280"

theme_utah_plot <- function() {
  theme_minimal(base_size = 13) +
    theme(
      text = element_text(color = UTAH_BLACK),
      plot.title = element_text(face = "bold", size = 14, color = UTAH_BLACK),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e5e7eb"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      aspect.ratio = 1
    )
}

# ---- Helpers ----
lb_to_kg <- function(lb) lb * 0.45359237
ft_to_m  <- function(ft) ft * 0.3048
minsec_to_minutes <- function(mins, secs) mins + secs / 60

vo2_rockport <- function(mass_kg, age_yrs, sex_code, time_min, hr_bpm)
  132.853 - (0.1692 * mass_kg) - (0.3877 * age_yrs) + (6.315 * sex_code) -
  (3.2469 * time_min) - (0.1565 * hr_bpm)

vo2_1_5_mile     <- function(time_min) 3.5 + 483 / time_min
vo2_queens_men   <- function(hr_bpm)   111.33 - (0.42   * hr_bpm)
vo2_queens_women <- function(hr_bpm)   65.81  - (0.1847 * hr_bpm)

# 6-Minute Walk Test
vo2_6mwt <- function(distance_m, rpe) 3.5 * (.882 + (.018 * distance_m) - (.161 * rpe))

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML(sprintf("
      .vo2-chart { width: 1200px; height: auto; }
      body { background: #ffffff; color: %s; }
      .card { border: 1px solid %s; border-radius: 12px; padding: 16px; margin-bottom: 16px; background: #fff; }
      .muted { color:%s; }
      .btn-primary, .btn-danger, .btn-default, .btn { border:none; }
      .btn-primary { background-color:%s; }
      .btn-danger { background-color:%s; }
      .nav-tabs>li>a { color:%s; font-weight:600; }
      .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
        color:#fff; background-color:%s; border:1px solid %s;
      }
      .nav-tabs>li>a:hover { color:%s; }
      h1, h2, h3, h4 { color:%s; }
      .header-logo {
      position: fixed;   /* stays at top-right even when scrolling */
      top: 25px;
      right: 30px;
      height: 48px;      /* tweak size as you like */
      z-index: 1000;     /* ensure it floats above content */
    }
    ", UTAH_BLACK, UTAH_RED, UTAH_GRAY, UTAH_RED, UTAH_RED, UTAH_BLACK, UTAH_RED, UTAH_RED, UTAH_RED, UTAH_BLACK)))
  ),
  titlePanel("Exercise Physiology Lab- VO₂max Estimator"),
  tags$img(src = "uofu_logo.png", alt = "University of Utah", class = "header-logo"),
  sidebarLayout(
    sidebarPanel(
      textInput("pid", "Participant ID (3 letters)", placeholder = "e.g., abc"),
      selectInput(
        "test_type", "Select Test",
        choices = c(
          "Rockport Walk Test" = "rockport",
          "1.5 Mile Run Test"  = "run15",
          "Queen's Step Test"  = "queens",
          "6-Minute Walk Test" = "walk6",
          "Measured VO2- Bruce Protocol" = "measured_bruce"
        )
      ),
      uiOutput("test_specific_inputs"),
      hr(),
      actionButton("submit", "Submit", class = "btn btn-primary"),
      br(), br(),
      tags$div(class="muted",
               "Notes:",
               tags$ul(
                 tags$li("Participant ID must be exactly 3 letters (case-insensitive)."),
                 tags$li("Enter time as minutes + seconds (seconds 0–59) where applicable."),
                 tags$li("Rockport weight is in pounds; converted to kg automatically (valid range 80–300 lb)."),
                 tags$li("6MWT distance is in feet (valid range 500–3000 ft); converted to meters automatically. RPE scale 6–20."),
                 tags$li("Measured VO2 (Bruce) requires VO2max, RER, peak HR, and yes/no flags for VO2max achieved and VO2 plateau.")
               )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Overview",
          div(class = "card",
              h3("What is VO₂max?"),
              p("VO₂max (maximal oxygen uptake) is the highest rate at which your body can consume oxygen during intense exercise, typically reported as milliliters of oxygen per kilogram of body mass per minute (ml·kg⁻¹·min⁻¹). It’s a strong indicator of aerobic fitness and endurance performance. Higher VO₂max values generally reflect better cardiorespiratory fitness, which is linked to improved health outcomes and lower risk for cardiovascular disease.")
          ),
          
          div(class = "card",
              h3("Tests included in this app"),
              p("This app estimates VO₂max using four field tests plus a measured option. Enter the required values on the left, then submit to compute and compare results."),
              
              h4("1) Rockport Walk Test"),
              tags$ul(
                tags$li("A 1-mile brisk walk; record time and end-exercise heart rate."),
                tags$li("Inputs: body weight (lb), age (years), sex (Female=0, Male=1), time (min:sec), HR (bpm)."),
                tags$li("Backend converts weight → kg and time → minutes.")
              ),
              
              h4("2) 1.5-Mile Run Test"),
              tags$ul(
                tags$li("Run 1.5 miles as fast as possible; record the finish time."),
                tags$li("Inputs: time (min:sec).")
              ),
              
              h4("3) Queen’s Step Test"),
              tags$ul(
                tags$li("A 3-minute step protocol followed by a 15-second recovery pulse converted to bpm."),
                tags$li("Inputs: sex (Female/Male), recovery HR (bpm).")
              ),
              
              h4("4) 6-Minute Walk Test (6MWT)"),
              tags$ul(
                tags$li("Walk as far as possible in 6 minutes; record total distance."),
                tags$li("Inputs: distance (feet), RPE (6–20 scale)."),
                tags$li("Backend converts distance → meters.")
              ),
              
              h4("5) Measured VO2- Bruce Protocol"),
              tags$ul(
                tags$li("Measured VO2max from a metabolic cart during a Bruce treadmill test."),
                tags$li("Inputs: VO2max, RER, peak HR, VO2max achieved (Yes/No), VO2 plateau (Yes/No)."),
                tags$li("VO2max value is stored directly as the VO₂ result.")
              )
          )
        ),
        tabPanel(
          "Data",
          div(class="card",
              h4("All Submissions"),
              div(
                style="margin-bottom:10px; display:flex; gap:10px; flex-wrap:wrap;",
                actionButton("remove_rows", "Remove selected", class = "btn btn-danger"),
                downloadButton("download_csv", "Download CSV"),
                downloadButton("download_rds", "Download RDS")
              ),
              br(),
              fluidRow(
                column(
                  4,
                  fileInput("upload_file", "Upload CSV or RDS", accept = c(".csv", ".rds"))
                ),
                column(
                  5,
                  radioButtons("upload_mode", "On upload:", inline = TRUE,
                               choices = c("Append" = "append", "Replace" = "replace"), selected = "append")
                )
              ),
              DTOutput("data_table")
          )
        ),
        tabPanel(
          "Results",
          div(class="card",
              h4("Latest Submission Result"),
              textOutput("vo2_result"),
              br(),
              uiOutput("vo2_details")
          ),
          div(class="card",
              h4("Scatter Correlations Between Measures"),
              plotOutput("scatter_pairs", height = "380px")
          )
        ),
        tabPanel(
          "VO₂max Chart",
          div(class="card",
              tags$img(
                src = "vo2max_chart.png",   # put this file in ./www/
                alt = "VO₂max reference values by sex",
                class = "vo2-chart"
              )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Autoincrement row id
  next_id <- reactiveVal(1)
  
  # In-memory store (no timestamp)
  base_df <- data.frame(
    id             = integer(),
    participant    = character(),
    test_type      = character(),   # rockport | run15 | queens | walk6 | measured_bruce
    sex_label      = character(),
    age            = numeric(),
    weight_lb      = numeric(),
    weight_kg      = numeric(),
    time_min       = numeric(),
    hr_bpm         = numeric(),
    distance_ft    = numeric(),     # 6MWT input
    distance_m     = numeric(),     # 6MWT derived
    rpe            = numeric(),     # 6MWT input
    rer            = numeric(),     # measured VO2 (Bruce)
    vo2max_achieved = character(),  # measured VO2 (Bruce)
    vo2_plateau     = character(),  # measured VO2 (Bruce)
    vo2            = numeric(),
    stringsAsFactors = FALSE
  )
  store <- reactiveValues(data = base_df)
  
  # ----- Dynamic inputs per test -----
  output$test_specific_inputs <- renderUI({
    req(input$test_type)
    switch(
      input$test_type,
      "rockport" = tagList(
        numericInput("mass_lb", "Body Weight (lb)", value = NA, min = 80, max = 300, step = 0.1),
        numericInput("age", "Age (years)", value = NA, min = 18, max = 65, step = 1),
        selectInput("sex_rc", "Sex (Rockport)", choices = c("Female (0)" = 0, "Male (1)" = 1)),
        fluidRow(
          column(6, numericInput("time_rc_min", "Time (minutes)", value = NA, min = 0, step = 1)),
          column(6, numericInput("time_rc_sec", "Time (seconds)", value = NA, min = 0, max = 59, step = 1))
        ),
        numericInput("hr_rc", "Heart Rate (bpm)", value = NA, min = 60, max = 200, step = 1)
      ),
      "run15" = tagList(
        fluidRow(
          column(6, numericInput("time_run_min", "Time (minutes)", value = NA, min = 0, step = 1)),
          column(6, numericInput("time_run_sec", "Time (seconds)", value = NA, min = 0, max = 59, step = 1))
        )
      ),
      "queens" = tagList(
        selectInput("sex_q", "Sex", choices = c("Female", "Male")),
        numericInput("hr_q", "Recovery Heart Rate (bpm)", value = NA, min = 60, max = 200, step = 1)
      ),
      "walk6" = tagList(
        numericInput("dist_ft", "Distance (feet)", value = NA, min = 500, max = 3000, step = 1),
        sliderInput("rpe6", "RPE (6–20)", min = 6, max = 20, value = 12, step = 1)
      ),
      "measured_bruce" = tagList(
        numericInput("vo2_meas", "VO2max (ml·kg⁻¹·min⁻¹)", value = NA, min = 5, max = 100, step = 0.1),
        numericInput("rer_meas", "RER (VCO₂/VO₂)", value = NA, min = 0.5, max = 2.5, step = 0.01),
        numericInput("hr_meas", "Peak Heart Rate (bpm)", value = NA, min = 60, max = 220, step = 1),
        radioButtons(
          "vo2max_achieved_meas", "VO2max achieved?",
          choices = c("Yes" = "Yes", "No" = "No"),
          inline = TRUE
        ),
        radioButtons(
          "vo2_plateau_meas", "VO2 plateau?",
          choices = c("Yes" = "Yes", "No" = "No"),
          inline = TRUE
        )
      )
    )
  })
  
  # ----- Submit -> data checks -> append record -----
  observeEvent(input$submit, {
    # Participant ID: exactly 3 letters; store uppercase
    pid_input <- input$pid
    pid_raw <- if (is.null(pid_input) || !nzchar(trimws(pid_input))) "" else trimws(pid_input)
    if (!grepl("^[A-Za-z]{3}$", pid_raw)) {
      showNotification("Participant ID must be exactly 3 letters (e.g., ABC).", type = "error"); return()
    }
    pid <- toupper(pid_raw)
    
    validate(need(input$test_type %in% c("rockport","run15","queens","walk6","measured_bruce"), "Choose a test."))
    
    if (input$test_type == "rockport") {
      req(input$mass_lb, input$age, input$sex_rc, input$time_rc_min, input$time_rc_sec, input$hr_rc)
      if (!is.finite(input$mass_lb) || input$mass_lb < 80 || input$mass_lb > 300) {
        showNotification("Body weight must be between 80 and 300 lb.", type = "error"); return()
      }
      if (!is.finite(input$age) || input$age < 18 || input$age > 65) {
        showNotification("Age must be between 18 and 65 years.", type = "error"); return()
      }
      if (!is.finite(input$hr_rc) || input$hr_rc < 60 || input$hr_rc > 200) {
        showNotification("Heart rate must be between 60 and 200 bpm.", type = "error"); return()
      }
      if (!is.finite(input$time_rc_min) || input$time_rc_min < 0 ||
          !is.finite(input$time_rc_sec) || input$time_rc_sec < 0 || input$time_rc_sec >= 60) {
        showNotification("Enter a valid time (seconds must be 0–59).", type = "error"); return()
      }
      
      mass_kg  <- lb_to_kg(input$mass_lb)
      time_min <- minsec_to_minutes(input$time_rc_min, input$time_rc_sec)
      vo2      <- vo2_rockport(mass_kg, input$age, as.numeric(input$sex_rc), time_min, input$hr_rc)
      rec <- data.frame(
        id              = next_id(),
        participant     = pid,
        test_type       = "rockport",
        sex_label       = ifelse(as.numeric(input$sex_rc) == 1, "Male (1)", "Female (0)"),
        age             = as.numeric(input$age),
        weight_lb       = as.numeric(input$mass_lb),
        weight_kg       = mass_kg,
        time_min        = time_min,
        hr_bpm          = as.numeric(input$hr_rc),
        distance_ft     = NA_real_,
        distance_m      = NA_real_,
        rpe             = NA_real_,
        rer             = NA_real_,
        vo2max_achieved = NA_character_,
        vo2_plateau     = NA_character_,
        vo2             = vo2
      )
      
    } else if (input$test_type == "run15") {
      req(input$time_run_min, input$time_run_sec)
      if (!is.finite(input$time_run_min) || input$time_run_min < 0 ||
          !is.finite(input$time_run_sec) || input$time_run_sec < 0 || input$time_run_sec >= 60) {
        showNotification("Enter a valid time (seconds must be 0–59).", type = "error"); return()
      }
      time_min <- minsec_to_minutes(input$time_run_min, input$time_run_sec)
      if (time_min <= 0) {
        showNotification("Total time must be > 0.", type = "error"); return()
      }
      vo2 <- vo2_1_5_mile(time_min)
      rec <- data.frame(
        id              = next_id(),
        participant     = pid,
        test_type       = "run15",
        sex_label       = "",
        age             = NA_real_,
        weight_lb       = NA_real_,
        weight_kg       = NA_real_,
        time_min        = time_min,
        hr_bpm          = NA_real_,
        distance_ft     = NA_real_,
        distance_m      = NA_real_,
        rpe             = NA_real_,
        rer             = NA_real_,
        vo2max_achieved = NA_character_,
        vo2_plateau     = NA_character_,
        vo2             = vo2
      )
      
    } else if (input$test_type == "queens") {
      req(input$sex_q, input$hr_q)
      if (!is.finite(input$hr_q) || input$hr_q < 60 || input$hr_q > 200) {
        showNotification("Heart rate must be between 60 and 200 bpm.", type = "error"); return()
      }
      vo2 <- if (input$sex_q == "Male") vo2_queens_men(input$hr_q) else vo2_queens_women(input$hr_q)
      rec <- data.frame(
        id              = next_id(),
        participant     = pid,
        test_type       = "queens",
        sex_label       = input$sex_q,
        age             = NA_real_,
        weight_lb       = NA_real_,
        weight_kg       = NA_real_,
        time_min        = NA_real_,
        hr_bpm          = as.numeric(input$hr_q),
        distance_ft     = NA_real_,
        distance_m      = NA_real_,
        rpe             = NA_real_,
        rer             = NA_real_,
        vo2max_achieved = NA_character_,
        vo2_plateau     = NA_character_,
        vo2             = vo2
      )
      
    } else if (input$test_type == "measured_bruce") {
      req(input$vo2_meas, input$rer_meas, input$hr_meas, input$vo2max_achieved_meas, input$vo2_plateau_meas)
      
      if (!is.finite(input$vo2_meas) || input$vo2_meas < 5 || input$vo2_meas > 100) {
        showNotification("VO2max must be between 5 and 100 ml·kg⁻¹·min⁻¹.", type = "error"); return()
      }
      if (!is.finite(input$rer_meas) || input$rer_meas < 0.5 || input$rer_meas > 2.5) {
        showNotification("RER must be between 0.50 and 2.50.", type = "error"); return()
      }
      if (!is.finite(input$hr_meas) || input$hr_meas < 60 || input$hr_meas > 220) {
        showNotification("Peak HR must be between 60 and 220 bpm.", type = "error"); return()
      }
      
      vo2 <- as.numeric(input$vo2_meas)
      
      rec <- data.frame(
        id              = next_id(),
        participant     = pid,
        test_type       = "measured_bruce",
        sex_label       = "",
        age             = NA_real_,
        weight_lb       = NA_real_,
        weight_kg       = NA_real_,
        time_min        = NA_real_,
        hr_bpm          = as.numeric(input$hr_meas),
        distance_ft     = NA_real_,
        distance_m      = NA_real_,
        rpe             = NA_real_,
        rer             = as.numeric(input$rer_meas),
        vo2max_achieved = as.character(input$vo2max_achieved_meas),
        vo2_plateau     = as.character(input$vo2_plateau_meas),
        vo2             = vo2
      )
      
    } else if (input$test_type == "walk6") {
      req(input$dist_ft, input$rpe6)
      if (!is.finite(input$dist_ft) || input$dist_ft < 500 || input$dist_ft > 3000) {
        showNotification("6MWT distance must be between 500 and 3000 feet.", type = "error"); return()
      }
      if (!is.finite(input$rpe6) || input$rpe6 < 6 || input$rpe6 > 20) {
        showNotification("RPE must be between 6 and 20.", type = "error"); return()
      }
      dist_m <- ft_to_m(as.numeric(input$dist_ft))
      vo2    <- vo2_6mwt(dist_m, as.numeric(input$rpe6))
      rec <- data.frame(
        id              = next_id(),
        participant     = pid,
        test_type       = "walk6",
        sex_label       = "",
        age             = NA_real_,
        weight_lb       = NA_real_,
        weight_kg       = NA_real_,
        time_min        = NA_real_,
        hr_bpm          = NA_real_,
        distance_ft     = as.numeric(input$dist_ft),
        distance_m      = dist_m,
        rpe             = as.numeric(input$rpe6),
        rer             = NA_real_,
        vo2max_achieved = NA_character_,
        vo2_plateau     = NA_character_,
        vo2             = vo2
      )
    } else {
      showNotification("Unknown test type.", type = "error"); return()
    }
    
    store$data <- rbind(store$data, rec)
    next_id(next_id() + 1L)
    showNotification("Submission saved.", type = "message")
    #updateTabsetPanel(session, "tabs", "Results")
  })
  
  # ----- Latest submission card (by highest id) -----
  latest_rec <- reactive({
    req(nrow(store$data) > 0)
    store$data[which.max(store$data$id), , drop = FALSE]
  })
  
  output$vo2_result <- renderText({
    lr <- latest_rec()
    paste0(format(round(lr$vo2, 1), nsmall = 1), " ml·kg⁻¹·min⁻¹")
  })
  
  output$vo2_details <- renderUI({
    lr <- latest_rec()
    tt <- lr$test_type
    det <-
      if (tt == "rockport") {
        sprintf("Rockport — %s | age=%s | weight=%0.1f lb (%0.1f kg) | time=%0.2f min | HR=%s bpm",
                lr$sex_label, lr$age, lr$weight_lb, lr$weight_kg, lr$time_min, lr$hr_bpm)
      } else if (tt == "run15") {
        sprintf("1.5 Mile Run — time=%0.2f min", lr$time_min)
      } else if (tt == "queens") {
        sprintf("Queen's Step — sex=%s | HR=%s bpm", lr$sex_label, lr$hr_bpm)
      } else if (tt == "measured_bruce") {
        sprintf("Measured VO2 (Bruce) — VO2max=%0.1f | RER=%0.2f | HR=%s bpm | VO2max achieved=%s | VO2 plateau=%s",
                lr$vo2, lr$rer, lr$hr_bpm, lr$vo2max_achieved, lr$vo2_plateau)
      } else {
        sprintf("6-Min Walk — distance=%0.0f ft (%0.1f m) | RPE=%s",
                lr$distance_ft, lr$distance_m, lr$rpe)
      }
    tagList(
      div(class="muted", paste0("Participant: ", lr$participant, " | Entry ID: ", lr$id)),
      div(det)
    )
  })
  
  # ----- Data Tab: table (hide weights), delete, download, upload -----
  display_df <- reactive({
    out <- store$data
    if (nrow(out) == 0) return(out)
    out <- out[order(out$participant, out$id, decreasing = TRUE), ]
    out[, c("id","participant","test_type","sex_label","age","time_min","hr_bpm",
            "distance_ft","rpe","rer","vo2max_achieved","vo2_plateau","vo2")]
  })
  
  output$data_table <- renderDT({
    dat <- display_df()
    dt <- datatable(
      dat,
      selection = "multiple",
      rownames  = FALSE,
      options   = list(pageLength = 10)
    )
    dt %>% formatRound(columns = c("time_min","rer","vo2"), digits = 1)
  })
  
  observeEvent(input$remove_rows, {
    dat <- display_df()
    sel_idx <- input$data_table_rows_selected
    if (length(sel_idx) == 0L) {
      showNotification("No rows selected.", type = "warning")
      return()
    }
    ids_to_remove <- dat$id[sel_idx]
    showModal(modalDialog(
      title = "Confirm deletion",
      paste0("Remove ", length(ids_to_remove), " selected row(s)? This cannot be undone."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Yes, remove", class = "btn btn-danger")
      )
    ))
    observeEvent(input$confirm_delete, {
      removeModal()
      keep <- !(store$data$id %in% ids_to_remove)
      store$data <- store$data[keep, , drop = FALSE]
      showNotification(paste0("Removed ", length(ids_to_remove), " row(s)."), type = "message")
    }, once = TRUE)
  })
  
  # Downloads include ALL columns (weights etc.)
  output$download_csv <- downloadHandler(
    filename = function() paste0("vo2_submissions_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content  = function(file) write.csv(store$data, file, row.names = FALSE)
  )
  output$download_rds <- downloadHandler(
    filename = function() paste0("vo2_submissions_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds"),
    content  = function(file) saveRDS(store$data, file)
  )
  
  # Upload (CSV/RDS) — auto on selection
  harmonize_cols <- function(df) {
    template <- names(base_df)
    for (nm in setdiff(template, names(df))) df[[nm]] <- NA
    df <- df[, template, drop = FALSE]
    df$id <- suppressWarnings(as.integer(df$id))
    char_cols <- c("participant","test_type","sex_label","vo2max_achieved","vo2_plateau")
    for (nm in char_cols) df[[nm]] <- as.character(df[[nm]])
    num_cols <- c("age","weight_lb","weight_kg","time_min","hr_bpm","distance_ft","distance_m","rpe","rer","vo2")
    for (nm in num_cols) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    # Uppercase 3-letter participant codes if present
    df$participant <- ifelse(grepl("^[A-Za-z]{3}$", df$participant), toupper(df$participant), df$participant)
    df
  }
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    path <- input$upload_file$datapath
    ext  <- tolower(tools::file_ext(input$upload_file$name))
    
    incoming <- tryCatch({
      if (ext == "csv")       read.csv(path, stringsAsFactors = FALSE)
      else if (ext == "rds")  readRDS(path)
      else stop("Unsupported file type.")
    }, error = function(e) {
      showNotification(paste("Failed to read file:", e$message), type = "error"); NULL
    })
    req(!is.null(incoming))
    
    incoming <- harmonize_cols(incoming)
    
    n_new <- nrow(incoming)
    if (n_new == 0) { showNotification("Upload contained 0 rows.", type = "warning"); return() }
    start_id <- next_id()
    incoming$id <- seq.int(start_id, length.out = n_new)
    next_id(start_id + n_new)
    
    if (isTruthy(input$upload_mode) && input$upload_mode == "replace") {
      store$data <- incoming
      showNotification(paste("Loaded", n_new, "rows (replace)."), type = "message")
    } else {
      store$data <- rbind(store$data, incoming)
      showNotification(paste("Loaded", n_new, "rows (append)."), type = "message")
    }
  })
  
  # ---- Scatter Pairs (pairwise inclusion; latest per person/test by highest id) ----
  output$scatter_pairs <- renderPlot({
    df_long <- store$data
    validate(need(nrow(df_long) > 0, "Need data to plot. Submit some tests."))
    
    df_long <- df_long[, c("participant","test_type","vo2","id")]
    ord <- order(df_long$participant, df_long$test_type, df_long$id, decreasing = TRUE)
    df_long <- df_long[ord, ]
    df_long <- df_long[!duplicated(df_long[, c("participant","test_type")]), ]
    
    tests_available <- sort(unique(df_long$test_type))
    validate(need(length(tests_available) >= 2, "Need at least two different tests to make pairwise plots."))
    
    pairs <- combn(tests_available, 2, simplify = FALSE)
    
    plots <- lapply(pairs, function(p) {
      a <- subset(df_long, test_type == p[1], select = c(participant, vo2))
      b <- subset(df_long, test_type == p[2], select = c(participant, vo2))
      names(a) <- c("participant","x")
      names(b) <- c("participant","y")
      merged <- merge(a, b, by = "participant", all = FALSE)
      if (nrow(merged) < 2) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("Not enough paired data for ", p[1], " vs ", p[2]),
                   size = 4) +
          theme_void() + theme_utah_plot()
      } else {
        r <- suppressWarnings(cor(merged$x, merged$y))
        lims <- range(c(merged$x, merged$y), na.rm = TRUE)
        if (diff(lims) == 0) lims <- lims + c(-0.5, 0.5)  # guard against flat data
        ggplot(merged, aes(x = x, y = y)) +
          geom_point(size = 2, alpha = 0.9, color = UTAH_BLACK) +
          geom_smooth(method = "lm", se = FALSE, color = UTAH_RED) +
          labs(
            x = paste0("VO₂max (", p[1], ")"),
            y = paste0("VO₂max (", p[2], ")"),
            title = paste0(p[1], " vs ", p[2], "  (r = ", sprintf("%.2f", r), ", n = ", nrow(merged), ")")
          ) +
          coord_equal(xlim = lims, ylim = lims, expand = TRUE) +  # <- same limits & 1:1 units
          theme_utah_plot()
      }
    })
    
    # Layout: up to 3 plots per row
    n <- length(plots)
    ncol <- if (n >= 3) 3 else n
    do.call(grid.arrange, c(plots, ncol = ncol))
  })
}

shinyApp(ui, server)