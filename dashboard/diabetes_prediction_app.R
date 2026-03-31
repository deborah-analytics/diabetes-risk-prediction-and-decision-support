# ---- RF Shiny app ----
library(shiny)
library(bslib)
library(ranger)                     

options(shiny.launch.browser = TRUE)

# Prevent thread-related crashes on Windows when stopping the app
Sys.setenv(OMP_THREAD_LIMIT = 1, OPENBLAS_NUM_THREADS = 1, MKL_NUM_THREADS = 1)

# Load model
if (!file.exists("rf_final.rds")) stop("rf_final.rds not found.")
rf_final   <- readRDS("rf_final.rds")
feat_cols  <- rf_final$forest$independent.variable.names
levels_out <- c("No","Pre","Yes")

# Input types (fallback to numeric if not listed)
TYPE_MAP <- list(
  HighBP="bin", HighChol="bin", BMI="num", Smoker="bin", Stroke="bin",
  HeartDiseaseorAttack="bin", PhysActivity="bin", Fruits="bin", Veggies="bin",
  HvyAlcoholConsump="bin", NoDocbcCost="bin",
  GenHlth="ord5", MentHlth="num", PhysHlth="num", DiffWalk="bin",
  Sex="bin", Age="ord13", Education="ord6", Income="ord8"
)

# Short help for each field (optional)
# Full, readable labels to show in the UI
LABEL_MAP <- list(
  HighBP               = "High blood pressure (ever told by a health professional)",
  HighChol             = "High cholesterol (ever told by a health professional)",
  BMI                  = "Body Mass Index (BMI)",
  Smoker               = "Smoker",
  Stroke               = "Stroke",
  HeartDiseaseorAttack = "Heart disease or Heart attack",
  PhysActivity         = "Physical Activity (outside regular job)",
  Fruits               = "Fruits",
  Veggies              = "Vegetables",
  HvyAlcoholConsump    = "Heavy Alcohol Consumption",
  NoDocbcCost          = "No Doctor due to cost",
  GenHlth              = "General Health rating",
  MentHlth             = "Mental Health",
  PhysHlth             = "Physical Health",
  DiffWalk             = "Difficulty Walking",
  Sex                  = "Sex",
  Age                  = "Age group",
  Education            = "Highest education level completed",
  Income               = "Annual household income band"
)

# Clear, plain-English help under each control
HELP_MAP <- list(
  HighBP="Select Yes if a doctor or other health professional has ever told the person they have high blood pressure.",
  HighChol="Select Yes if ever told by a health professional they have high cholesterol.",
  BMI="Enter BMI (e.g., 27.5).",
  Smoker="Yes if the person has smoked at least 100 cigarettes in their lifetime; otherwise No.",
  Stroke="Yes if ever told by a health professional they had a stroke.",
  HeartDiseaseorAttack="Yes if ever told they had coronary heart disease (CHD) or a myocardial infarction (heart attack).",
  PhysActivity="Yes if any leisure-time physical activity/exercise in the past 30 days (outside regular job).",
  Fruits="Yes if they usually eat fruit at least once per day; otherwise No.",
  Veggies="Yes if they usually eat vegetables at least once per day; otherwise No.",
  HvyAlcoholConsump="Yes if meets BRFSS heavy drinking threshold (men >14 drinks/week; women >7 drinks/week).",
  NoDocbcCost="Yes if in the past 12 months they needed to see a doctor but could not because of cost.",
  GenHlth="1 = Excellent, 2 = Very good, 3 = Good, 4 = Fair, 5 = Poor.",
  MentHlth="How many days in the last 30 was mental health not good? (whole number 0–30).",
  PhysHlth="How many days in the last 30 was physical health not good? (whole number 0–30).",
  DiffWalk="Yes if serious difficulty walking or climbing stairs.",
  Sex="Female = 0, Male = 1 (use the code used in training).",
  Age="1=18–24, 2=25–29, …, 13=80+.",
  Education="1=None/Kindergarten, 2=Elementary, 3=Some high school, 4=High school grad, 5=Some college, 6=College graduate.",
  Income="1=<10k, 2=10–15k, 3=15–20k, 4=20–25k, 5=25–35k, 6=35–50k, 7=50–75k, 8=>75k."
)

# ----- Choice vectors -----
BIN_CHOICES <- c("No (0)" = 0, "Yes (1)" = 1)
SEX_CHOICES <- c("Female (0)" = 0, "Male (1)" = 1)

GENHLTH_CHOICES <- setNames(1:5, c(
  "Excellent (1)", "Very good (2)", "Good (3)", "Fair (4)", "Poor (5)"
))

AGE_CHOICES <- setNames(1:13, c(
  "18–24 (1)","25–29 (2)","30–34 (3)","35–39 (4)","40–44 (5)",
  "45–49 (6)","50–54 (7)","55–59 (8)","60–64 (9)","65–69 (10)",
  "70–74 (11)","75–79 (12)","80+ (13)"
))

EDU_CHOICES <- setNames(1:6, c(
  "None/Kindergarten (1)","Elementary (2)","Some high school (3)",
  "High school grad (4)","Some college (5)","College graduate (6)"
))

INC_CHOICES <- setNames(1:8, c(
  "<10k (1)","10–15k (2)","15–20k (3)","20–25k (4)",
  "25–35k (5)","35–50k (6)","50–75k (7)",">75k (8)"
))


safe_normalize <- function(df_probs){
  m <- as.matrix(df_probs); rs <- rowSums(m); rs[!is.finite(rs) | rs==0] <- 1
  as.data.frame(sweep(m, 1, rs, "/"), check.names = FALSE)
}

# ---- Tunable decision cut-offs for "What to do next" ----
THRESH_YES <- 0.20   # P(Diabetes) ≥ 0.20 → diabetes path
THRESH_PRE <- 0.35   # P(Prediabetes) ≥ 0.35 → prediabetes path

action_from_probs <- function(p_no, p_pre, p_yes){
  if (p_yes >= THRESH_YES) {
    "Raised likelihood of diabetes — arrange confirmatory HbA1c/FPG and clinical assessment."
  } else if (p_pre >= THRESH_PRE) {
    "Increased likelihood of prediabetes — order HbA1c/FPG; consider referral to a structured lifestyle-change programme."
  } else {
    "Low likelihood; not diagnostic. Use clinical judgement."
  }
}


# Hide "Close app" button when deployed to a Shiny server
IS_DEPLOYED <- nzchar(Sys.getenv("SHINY_SERVER_VERSION")) || nzchar(Sys.getenv("RS_CONNECT_SERVER"))



ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel(
    tagList(
      div(id = "app_title", "Diabetes Status Prediction from Routine Health Indicators"),
      div(id = "app_subtitle", "A Shiny decision-support tool")
    ),
    windowTitle = "Diabetes Status Prediction – Shiny decision-support tool"
  ),
  
  tags$head(
    tags$style(HTML("
    /* Title + subtitle */
    #app_title { margin-bottom:.25rem; }
    #app_subtitle { color:#6c757d; font-size:1rem; }

    /* Mobile: shrink title a bit */
    @media (max-width:576px){
      #app_title { font-size:1.35rem; line-height:1.2; }
      #app_subtitle { font-size:.95rem; }
    }

    /* What to do next box */
    #what_next_box { 
      border-left: 3px solid var(--bs-border-color); 
      padding-left: 10px; 
      margin-top: .5rem; 
    }
    #what_next_box p { 
      color: #6c757d;
      margin-bottom: 0; 
    }
    #what_next_box h5 { 
      margin-bottom: .4rem; 
    }

    /* NEW: tint border based on predicted class */
    #what_next_box.status-no  { border-left-color: rgba(var(--bs-success-rgb), .35); }
    #what_next_box.status-pre { border-left-color: rgba(var(--bs-warning-rgb), .45); }
    #what_next_box.status-yes { border-left-color: rgba(var(--bs-danger-rgb),  .40); }
  ")),
    
    tags$script(HTML("Shiny.addCustomMessageHandler('scrollToResult', function(){
      var el = document.getElementById('status_card');
      if (el) el.scrollIntoView({behavior:'smooth', block:'start'});
    });")),
    tags$script(HTML("$(document).on('click', '#quit_app', function(e){
      e.preventDefault();
      if (window.confirm('Close the application?')) {
        Shiny.setInputValue('quit_confirmed', Date.now(), {priority:'event'});
      }
    });"))
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      h4("Patient inputs"),
      uiOutput("dyn_inputs"),
      hr(),
      div(
        actionButton("predict_one", "Predict", class = "btn btn-primary"),
        actionButton("reset_inputs", "Reset", class = "btn btn-secondary", style = "margin-left:8px;"),
        if (!IS_DEPLOYED)
          actionButton(
            "quit_app",
            label = tagList(shiny::icon("power-off"), "Close app"),
            class = "btn btn-outline-danger",
            style = "margin-left:8px;"
          )
      ),
      br(),
      conditionalPanel(
        "output.has_pred",
        downloadButton("download_probs","Download result (CSV)", class="btn btn-outline-secondary")
      )
    ),
    
    mainPanel(
      h4("Result"),
      div(class = "mb-2",
          actionLink("about_tool",
                     label = tagList(icon("info-circle"), "About this tool / guidance"))
      ),
      uiOutput("status_card"),
      uiOutput("generated_at"),
      br(),
      uiOutput("what_next"),
      hr(),
      tags$small(HTML(
        "<b>Important:</b> This tool provides probability estimates and is <b>not diagnostic</b>. 
     Follow local clinical guidance (e.g., NICE in the UK) for confirmatory testing (HbA1c 
     or fasting plasma glucose) and referral to structured lifestyle-change programmes. 
     Use alongside clinical judgement."
      ))
    )
  )
)


`%||%` <- function(x, y) if (is.null(x)) y else x



#---------- Server -------------

server <- function(input, output, session){
  last_pred <- reactiveVal(NULL)
  
  output$has_pred <- reactive(!is.null(last_pred()))
  outputOptions(output, "has_pred", suspendWhenHidden = FALSE)
  
  
  # quit cleanly when the browser tab/window closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$quit_confirmed, {
    stopApp()
  })
  
  # Build inputs (fallback to numeric if not in TYPE_MAP)
  output$dyn_inputs <- renderUI({
    widgets <- lapply(feat_cols, function(nm){
      kind <- if (!is.null(TYPE_MAP[[nm]])) TYPE_MAP[[nm]] else "num"
      id   <- paste0("in_", nm)
      lab  <- LABEL_MAP[[nm]] %||% nm
      
      ctrl <- switch(kind,
                     "bin" = radioButtons(
                       inputId = id, label = lab,
                       choices = if (nm == "Sex") SEX_CHOICES else BIN_CHOICES,
                       selected = "0", inline = TRUE
                     ),
                     "num" = {
                       if (nm == "BMI") {
                         numericInput(id, lab, value = 27.5, min = 10, max = 60, step = 0.1)
                       } else if (nm %in% c("MentHlth","PhysHlth")) {
                         sliderInput(id, lab, min = 0, max = 30, value = 0, step = 1)
                       } else {
                         numericInput(id, lab, value = 0, step = 0.1)
                       }
                     },
                     "ord5"  = selectInput(id, lab, choices = if (nm=="GenHlth") GENHLTH_CHOICES else setNames(1:5,1:5), selected = 3),
                     "ord6"  = selectInput(id, lab, choices = if (nm=="Education") EDU_CHOICES else setNames(1:6,1:6), selected = 3),
                     "ord8"  = selectInput(id, lab, choices = if (nm=="Income") INC_CHOICES else setNames(1:8,1:8), selected = 4),
                     "ord13" = selectInput(id, lab, choices = if (nm=="Age") AGE_CHOICES else setNames(1:13,1:13), selected = 7),
                     numericInput(id, lab, value = 0, step = 0.1)
      )
      
      help <- if (!is.null(HELP_MAP[[nm]])) tags$small(tags$em(HELP_MAP[[nm]])) else NULL
      tagList(ctrl, help, tags$hr(style="margin:6px 0;"))
    })
    do.call(tagList, widgets)
  })
  
  
  observeEvent(input$about_tool, {
    showModal(modalDialog(
      title = "About this tool / guidance",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tagList(
        p("This prototype estimates the likelihood of no diabetes / prediabetes / diabetes from survey health indicators. It is not a diagnostic device."),
        tags$ul(
          tags$li("If the result is “Prediabetes”: arrange confirmatory testing (HbA1c or fasting plasma glucose) and consider referral to a structured lifestyle-change programme."),
          tags$li("If the result is “Diabetes”: arrange prompt clinical assessment and confirmatory blood testing."),
          tags$li("If the result is “No diabetes”: continue routine care; reassess if clinical concern persists.")
        ),
        tags$hr(),
        tags$small(
          "Follow local clinical guidance (e.g., NICE in the UK). ",
          tags$a(
            href = "https://www.nice.org.uk/", target = "_blank",
            "Open NICE website ",
            shiny::icon("external-link-alt", lib = "font-awesome")
          )
        ),
        tags$hr(),
        tags$small(
          style = "color:#6c757d;font-style:italic;",   # grey + italic
          "Privacy notice: This prototype does not store or transmit any patient inputs. ",
          "All computations are performed in-memory within the Shiny session and cleared ",
          "when the session ends. No personally identifiable information is retained."
        )
      )
    ))
  })
  
  
  # Reset
  observeEvent(input$reset_inputs, {
    for (nm in feat_cols) {
      kind <- if (!is.null(TYPE_MAP[[nm]])) TYPE_MAP[[nm]] else "num"
      id   <- paste0("in_", nm)
      
      if (kind == "bin") {
        # radioButtons expect character values ("0"/"1")
        updateRadioButtons(session, id, selected = "0")
      } else if (nm %in% c("MentHlth","PhysHlth")) {
        updateSliderInput(session, id, value = 0)
      } else if (grepl("^ord", kind)) {
        k <- as.integer(gsub("\\D","", kind)); mid <- ceiling(k/2)
        # selectInput expects character too
        updateSelectInput(session, id, selected = as.character(mid))
      } else {
        updateNumericInput(session, id, value = if (nm == "BMI") 27.5 else 0)
      }
    }
    
    # clear any previous result
    last_pred(NULL)
    output$status_card  <- renderUI(NULL)
    output$generated_at <- renderUI(NULL)
    output$what_next    <- renderUI(NULL)
  }, ignoreInit = TRUE)
  
  
  # Build one-row DF
  collect_row <- function(){
    vals <- lapply(feat_cols, function(nm){
      v <- input[[paste0("in_", nm)]]
      kind <- if (!is.null(TYPE_MAP[[nm]])) TYPE_MAP[[nm]] else "num"
      if (kind == "bin") return(as.numeric(v))                      # radios return "0"/"1"
      if (nm %in% c("MentHlth","PhysHlth")) return(as.numeric(v))  # slider 0–30
      as.numeric(ifelse(is.null(v) || is.na(v), 0, v))              # numeric/selects
    })
    names(vals) <- feat_cols
    as.data.frame(vals, check.names = FALSE)
  }
  
  
  observeEvent(input$predict_one, {
    tryCatch({
      X <- collect_row()
      X <- X[, feat_cols, drop = FALSE]
      validate(
        need(ncol(X) == length(feat_cols), "Input error: some features are missing."),
        need(all(sapply(X, is.finite)),     "Input error: non-numeric values detected.")
      )
      
      rf_prob <- predict(rf_final, data = X, type = "response", num.threads = 1)$predictions
      rf_prob <- rf_prob[, levels_out, drop = FALSE]
      rf_prob <- safe_normalize(rf_prob)
      
      cls <- colnames(rf_prob)[max.col(as.matrix(rf_prob), ties.method = "first")]
      
      
      #decide action text from probabilities
      act_text <- action_from_probs(rf_prob[1,"No"], rf_prob[1,"Pre"], rf_prob[1,"Yes"])
      
      
      p_pre <- rf_prob[,"Pre"];  p_yes <- rf_prob[,"Yes"]
      
      
      # Big verdict line
      # Big verdict line (with relevant probability under the badge)
      output$status_card <- renderUI({
        label <- switch(cls, "No"="No diabetes", "Pre"="Prediabetes", "Yes"="Diabetes")
        col   <- if (cls=="No") "#198754" else if (cls=="Pre") "#FFC107" else "#DC3545"
        
        # Build a single-line probability for the predicted class
        rel_line <- switch(
          cls,
          "Yes" = sprintf("Estimated probability of diabetes: %.1f%%.", 100 * p_yes),
          "Pre" = sprintf("Estimated probability of prediabetes: %.1f%%.", 100 * p_pre),
          "No"  = NULL
        )
        
        div(
          id = "status_card",
          tags$h4("Predicted status"),
          tags$span(
            style = paste0(
              "display:inline-block;padding:.4rem .7rem;border-radius:.5rem;",
              "background:", col, ";color:#fff;font-weight:600;"
            ),
            label
          ),
          if (!is.null(rel_line))
            tags$div(style = "margin-top:6px;color:#6c757d;", rel_line)
        )
      })
      
      
      # Action text
      output$what_next <- renderUI({
        cls_class <- switch(cls, "No"="status-no", "Pre"="status-pre", "Yes"="status-yes")
        div(
          id = "what_next_box", class = cls_class,
          h5("What to do next"),
          p(act_text)
        )
      })
      
      
      session$sendCustomMessage("scrollToResult", list())
      
      # ---- Timestamp under the verdict ----
      ts <- format(Sys.time(), "%Y-%m-%d %H:%M")
      output$generated_at <- renderUI(tags$small(tags$em(paste("Generated:", ts))))
      
      
      # ---- Store for CSV (now with timestamp) ----
      rel_prob <- switch(cls,
                         "Yes" = p_yes,
                         "Pre" = p_pre,
                         "No"  = rf_prob[,"No"]
      )
      
      out <- data.frame(
        Predicted_Class = cls,
        Relevant_Prob = round(100 * rel_prob, 1),  # percent
        Action          = act_text,
        GeneratedAt     = ts,
        check.names     = FALSE
      )
      
      
      last_pred(out)
      
      
    }, error = function(e){
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)
  
  output$download_probs <- downloadHandler(
    filename = function() paste0("prediction_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content  = function(file) {
      res <- last_pred(); if (is.null(res)) stop("No result yet. Click Predict first.")
      write.csv(res, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)