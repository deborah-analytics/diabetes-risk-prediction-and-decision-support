# ==========================
# Smart Glucose Care 
# ==========================


library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)



# ====== Clinical Logic ======
generate_recommendation <- function(glucose, hba1c, bmi, medication, is_new_diagnosis) {
  recommendations <- c()
  
  if (is_new_diagnosis) {
    recommendations <- c(recommendations, 
                         "📚 Offer structured education program at diagnosis (NG28 1.2.1)",
                         "🥗 Provide personalized lifestyle advice on diet, exercise, and weight management (NG28 1.3)")
    if (glucose > 10 | hba1c >= 7.5) {
      recommendations <- c(recommendations, "💊 Initiate Metformin therapy due to elevated glucose/HbA1c (NG28 1.7.2)")
    } else {
      recommendations <- c(recommendations, "🕒 Monitor without medication initially. Review glucose control in 3–6 months.")
    }
  } else {
    if (hba1c > 7.5 & bmi >= 25) {
      recommendations <- c(recommendations, "⚖️ Weight loss target: 5–10% (NG28 1.3.5)")
    }
    if (glucose > 10 | hba1c > 7.5) {
      dietary_advice <- c(
        "🍽️ Increase high-fiber, low-GI carbohydrates (NG28 1.3.3)",
        "🍽️ Limit saturated fats to <10% total energy (NG28 1.3.3)",
        "🍽️ Oily fish 1–2x/week (NG28 1.3.3)"
      )
      recommendations <- c(recommendations, dietary_advice)
    }
    if (medication == "Metformin" & glucose > 7) {
      recommendations <- c(recommendations, "💊 Consider adding SGLT2 inhibitor if glucose remains uncontrolled (NG28 1.8.2)")
    }
  }
  return(unique(recommendations))
}



# ====== UI ======
ui <- fluidPage(
  theme = shinytheme("yeti"),
  responsive = TRUE,
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    body {
      background-image: url('https://www.transparenttextures.com/patterns/white-wall-3.png');
      font-family: 'Inter', sans-serif;
      overflow-x: hidden;
    }
    .gradient-header {
  background: linear-gradient(to right, #004e92, #000428);
  color: white;
  padding: 30px 20px 20px;
  border-radius: 30px;
  margin: 0 auto 20px auto;
  box-shadow: 0 8px 16px rgba(0,0,0,0.2);
  text-align: center;
  width: 100%;
    }
    .input-card, .output-card, .tab-card {
      background-color: white;
      padding: 20px;
      border-radius: 16px;
      box-shadow: 0 6px 14px rgba(0,0,0,0.08);
      margin-bottom: 20px;
      margin-left: auto;
      margin-right: auto;
      max-width: 800px;
      transition: all 0.3s ease-in-out;
    }
    .input-card:hover, .output-card:hover, .tab-card:hover {
      transform: translateY(-4px);
      box-shadow: 0 12px 24px rgba(0,0,0,0.12);
    }
    .recommendation-card, .coding-card, .summary-card {
      padding: 20px;
      border-radius: 12px;
      margin-bottom: 20px;
    }
    .recommendation-card {
      background-color: #e3f2fd;
      border-left: 5px solid #2196f3;
    }
    .coding-card {
      background-color: #ede7f6;
      border-left: 5px solid #673ab7;
    }
    .summary-card {
      background-color: #e8f5e9;
      border-left: 5px solid #4caf50;
    }
    h4.section-title {
      font-weight: bold;
      color: #004e92;
      margin-bottom: 15px;
      text-align: center;
    }
    .btn-primary, .btn-outline-secondary {
      font-weight: 600;
      border-radius: 10px;
    }
    .tab-content {
      margin-top: 10px;
    }
    .footer {
      font-size: 12px;
      color: #666;
      text-align: center;
      margin: 30px 0;
    }
    
h2.main-title {
  font-family: 'Poppins', sans-serif;
  font-size: 38px;
  font-weight: 800;
  background: linear-gradient(90deg, #00c6ff, #00d2ff); /* lighter fresh gradient */
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
  margin-bottom: 10px;
}

h4.section-heading {
  font-family: 'Poppins', sans-serif;
  font-size: 26px;
  font-weight: 700;
  color: #004e92;
  border-bottom: 2px solid #004e92;
  padding-bottom: 8px;
  margin-bottom: 20px;
  text-align: center;
}

.nav-tabs > li > a {
  color: #004e92; /* Calm navy blue */
  font-weight: 600;
  font-size: 16px;
  border-radius: 12px 12px 0 0; /* Rounded top corners */
  padding: 10px 20px;
  margin-right: 5px;
  transition: background-color 0.3s, color 0.3s;
}

.nav-tabs > li.active > a, 
.nav-tabs > li.active > a:focus, 
.nav-tabs > li.active > a:hover {
  color: white;
  background: linear-gradient(to right, #004e92, #000428);
  border: none;
  border-radius: 12px 12px 0 0;
}

.nav-tabs > li > a:hover {
  background: rgba(0, 78, 146, 0.1);
  color: #00264d;
}

  "))
    
  ),
  
  div(class = "gradient-header",
      tags$h2(style = "font-weight: bold; color: white; font-size: 40px; margin-bottom: 8px;", 
              "🩺 Smart Glucose Care – Type 2 Diabetes Decision Support System"),
      tags$h5(style = "color: #e0e0e0; font-weight: 400; font-size: 18px; margin-bottom: 4px;",
              "Built with NICE NG28 · 2025 · R + Shiny"),
      tags$h6(style = "color: #cccccc; font-weight: 300; font-size: 16px;",
              "Rule-Based Recommendations · Virtual Monitoring Support")
  ),
  
  tabsetPanel(
    tabPanel(
      title = div(icon("sign-in-alt"), span("Patient Entry", style = "margin-left: 8px;")),
      div(class = "tab-card",
          h4(class = "section-heading", "Enter Patient Data"),
          numericInput("glucose", "Fasting Glucose (mmol/L)", value = 7.5, min = 2, max = 30),
          numericInput("hba1c", "HbA1c (%)", value = 6.5, min = 4, max = 20),
          numericInput("bmi", "BMI", value = 28, min = 15, max = 50),
          selectInput("medication", "Current Medication",
                      choices = c("None", "Metformin", "Sulfonylurea", "SGLT2", "Insulin")),
          switchInput("new_diagnosis", "New Diagnosis?", FALSE),
          actionButton("run", "Generate Recommendation", class = "btn btn-primary mt-3")
      )
    ),
    
    tabPanel(
      title = div(icon("book-medical"), span("NICE Guidelines", style = "margin-left: 8px;")),
      div(class = "tab-card",
          h4(class = "section-heading", "NICE Guidelines Overview"),
          p("This decision support system aligns with the ",
            a("NICE NG28 guidelines", href = "https://www.nice.org.uk/guidance/ng28", target = "_blank"),
            " for Type 2 Diabetes management."),
          tags$ul(
            tags$li("Focus on lifestyle advice and structured education."),
            tags$li("Consider medication escalation based on glucose control."),
            tags$li("Individualized care plans and support."),
            tags$li("Weight management and dietary guidance emphasized.")
          ),
          br(),
          h4("How Patient Inputs Align with NICE NG28"),
          tags$table(class = "table table-striped",
                     tags$thead(
                       tags$tr(
                         tags$th("Input Field"),
                         tags$th("NICE NG28 Connection")
                       )
                     ),
                     tags$tbody(
                       tags$tr(tags$td("🧪 Fasting Glucose (mmol/L)"), tags$td("Assesses hyperglycemia or hypoglycemia risk (NG28 Section 1.6)")),
                       tags$tr(tags$td("🩸 HbA1c (%)"), tags$td("Glycemic control marker guiding therapy adjustments (NG28 Section 1.6.1)")),
                       tags$tr(tags$td("⚖️ BMI"), tags$td("Guides individualized weight loss advice (NG28 Section 1.3)")),
                       tags$tr(tags$td("💊 Current Medication"), tags$td("Informs potential escalation of therapy (NG28 Section 1.8)")),
                       tags$tr(tags$td("📅 New Diagnosis?"), tags$td("Triggers offer of structured education programs early (NG28 Section 1.2)"))
                     )
          )
      )
    ),
    
    tabPanel(
      title = div(icon("info-circle"), span("About this App", style = "margin-left: 8px;")),
      div(class = "tab-card",
          h4(class = "section-heading", "About Smart Glucose Care"),
          tags$ul(
            tags$li("🩺 Clinical Decision Support for Type 2 Diabetes (NICE NG28)"),
            tags$li("📈 Personalized, dynamic recommendations based on patient data"),
            tags$li("📄 Automatic ICD-10 clinical coding guidance"),
            tags$li("💬 Supports virtual remote monitoring and therapy review"),
            tags$li("🛡️ Educational concept only — no personal data stored")
          ),
          br(),
          p(em("Built with ❤️ using R + Shiny · 2025"))
      )
    )
  ),
  
  div(class = "footer", "© 2025 · Smart Glucose Care · Deborah Adebayo"),
  
  bsModal(
    "modalOutput",
    div(style = "padding:10px;",
        h2(
          "📋 Clinical Decision Support Summary",
          style = "
          font-family: 'Poppins', sans-serif;
          font-size: 32px;
          font-weight: 700;
          color: #004e92;
          margin-bottom: 15px;
          text-align: center;
          letter-spacing: 0.5px;
        "
        )
    ),
    "run",
    size = "large",
    
    div(class = "recommendation-card",
        h4(class = "section-title", "💡 Recommendations"),
        uiOutput("recommendation_box")
    ),
    
    div(class = "coding-card",
        h4(class = "section-title", "🧾 Clinical Coding (ICD-10)"),
        textOutput("icd10_code")
    ),
    
    div(class = "summary-card",
        h4(class = "section-title", "📋 Patient Summary"),
        uiOutput("input_summary_box")
    ),
    
    div(style = "text-align:center; margin-top:20px;",
        p(style="font-size:12px; color:#666;", "Summary generated based on NICE NG28 guidelines."),
        p(style="font-size:12px; color:#666;", paste("Generated on:", Sys.Date()))
    ),
    
    div(style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 10px;",
        downloadButton("downloadTxt", "Download Summary", class = "btn btn-outline-secondary ml-2")
    )
  )
)


# ====== Server ======
server <- function(input, output, session) {
  
  # Initialize tooltips 
  observe({
    tooltip_info <- list(
      glucose = "Enter the patient's most recent fasting plasma glucose level in mmol/L.",
      hba1c = "Provide the most recent glycated hemoglobin (HbA1c) percentage result.",
      bmi = "Record the patient's Body Mass Index (BMI) for personalized weight management advice.",
      medication = "Select the main glucose-lowering agent the patient is currently using.",
      new_diagnosis = "Indicate whether the patient has been newly diagnosed with Type 2 Diabetes."
    )
    
    lapply(names(tooltip_info), function(id) {
      addTooltip(session, id = id, title = tooltip_info[[id]], placement = "right", options = list(container = "body"))
    })
  })
  
  observeEvent(input$run, {
    
    output$recommendation_box <- renderUI({
      recs <- generate_recommendation(
        input$glucose, input$hba1c, input$bmi, input$medication, input$new_diagnosis
      )
      
      # Group by types
      education <- recs[grepl("education", recs, ignore.case = TRUE)]
      lifestyle  <- recs[grepl("fiber|fat|fish|weight loss", recs, ignore.case = TRUE)]
      medication <- recs[grepl("Metformin|Monitor without", recs, ignore.case = TRUE) | grepl("Initiate", recs, ignore.case = TRUE)]
      
      if (length(education) + length(lifestyle) + length(medication) == 0) {
        tags$p("✅ Patient appears stable. Continue current management plan and routine monitoring as per NICE NG28.")
      } else {
        tagList(
          if (length(education) > 0) {
            div(
              h5("📚 Education"),
              tags$ul(lapply(education, function(x) tags$li(x)))
            )
          },
          if (length(lifestyle) > 0) {
            div(
              h5("🍽️ Lifestyle Advice"),
              tags$ul(lapply(lifestyle, function(x) tags$li(x)))
            )
          },
          if (length(medication) > 0) {
            div(
              h5("💊 Medication Advice"),
              tags$ul(lapply(medication, function(x) tags$li(x)))
            )
          }
        )
      }
    })
    
    output$icd10_code <- renderText({
      if (input$glucose > 11.1) {
        "ICD-10: R73.9 (Hyperglycemia)"
      } else if (input$glucose < 3.9) {
        "ICD-10: E16.2 (Hypoglycemia)"
      } else {
        "ICD-10: E11 (Type 2 Diabetes Mellitus without complications)"
      }
    })
    
    output$input_summary_box <- renderUI({
      tagList(
        p("🧪 Fasting Glucose: ", input$glucose, " mmol/L"),
        p("🩸 HbA1c: ", input$hba1c, "%"),
        p("⚖️ BMI: ", input$bmi),
        p("💊 Medication: ", input$medication),
        p("📅 New Diagnosis?: ", ifelse(input$new_diagnosis, "Yes", "No"))
      )
    })
  })
  
  output$downloadTxt <- downloadHandler(
    filename = function() {
      paste0("SmartGlucoseCare_Summary_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".txt")
    },
    content = function(file) {
      recs <- generate_recommendation(input$glucose, input$hba1c, input$bmi, input$medication, input$new_diagnosis)
      recommendation_text <- if (length(recs) > 0) paste("-", recs, collapse = "\n- ") else "✅ Patient appears stable. Continue current management plan and routine monitoring as per NICE NG28."
      icd_code <- if (input$glucose > 11.1) "ICD-10: R73.9 (Hyperglycemia)" else if (input$glucose < 3.9) "ICD-10: E16.2 (Hypoglycemia)" else "ICD-10: E11 (Type 2 Diabetes Mellitus without complications)"
      
      summary <- paste(
        "🌟 SMART GLUCOSE CARE – CLINICAL SUMMARY",
        "-----------------------------",
        "",
        paste("🧪 Fasting Glucose:", input$glucose, "mmol/L"),
        paste("🩸 HbA1c:", input$hba1c, "%"),
        paste("⚖️ BMI:", input$bmi),
        paste("💊 Medication:", input$medication),
        paste("📅 New Diagnosis?:", ifelse(input$new_diagnosis, "Yes", "No")),
        "",
        "💡 Recommendations:",
        recommendation_text,
        "",
        "🧾 Clinical Coding:",
        icd_code,
        "",
        "-----------------------------",
        paste("Generated on:", Sys.Date()),
        sep = "\n"
      )
      
      writeLines(summary, file)
    }
  )
}

# ====== Run the app ======
shinyApp(ui, server)
