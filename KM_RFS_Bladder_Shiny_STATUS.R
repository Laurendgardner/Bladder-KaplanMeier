library(shiny)
library(ggsurvfit)
library(gtsummary)
library(survival)

# Read "bladder.tsv" file into a data frame named "bladder_data"
bladder_data <- read.table(file = "/Users/laurengardner/Projects/KP_BladderProject/bladder.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Create a new column named "NewRecurrenceOutcomeTable"
bladder_data$NewRecurrenceOutcomeTable <- "NA"

# Assign status code for new column
bladder_data[which(bladder_data$recurrence_outcome_JD == "Yes"), "NewRecurrenceOutcomeTable"] <- 1
bladder_data[which(bladder_data$recurrence_outcome_JD == "No"), "NewRecurrenceOutcomeTable"] <- 0

# Create a new column named "NewRecurrenceOutcomeTable_num" with numerical character values
bladder_data$NewRecurrenceOutcomeTable_num <- as.numeric(as.character(bladder_data$NewRecurrenceOutcomeTable))

# Create a new column named "clinical_os_JD_num" with numerical character values
bladder_data$clinical_os_JD_num <- as.numeric(as.character(bladder_data$clinical_os_JD))

# UI
ui <- fluidPage(
  titlePanel("Kaplan-Meier Survival Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("strata_col", "Select Stratification Column:", choices = colnames(bladder_data), selected = "tstage_c_JD")
    ),
    mainPanel(
      plotOutput("kmPlot"),
      verbatimTextOutput("oneYearEstimate"),
      verbatimTextOutput("medianEstimate"),
      tableOutput("oneYearTable"),
      tableOutput("medianTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$kmPlot <- renderPlot({
    
    # Create Kaplan-Meier plot
    SurvFeature = input$strata_col 
    form <- as.formula(paste0("Surv(clinical_os_JD_num,NewRecurrenceOutcomeTable_num) ~ ",SurvFeature))
    print(form)
    print(SurvFeature)
    print(input$strata_col)
    
    survfit2(form,data = bladder_data, type="kaplan-meier") %>% 
    
      ggsurvfit() +
      labs(
        x = "Months",
        y = "Recurrence Free Survival Probability"
      ) + 
      add_confidence_interval() +
      add_risktable()
  })
  
  # Estimating 1-year survival (weeks)
  output$oneYearEstimate <- renderPrint({
    SurvFeature = input$strata_col
    form <- as.formula(paste0("Surv(clinical_os_JD_num,NewRecurrenceOutcomeTable_num) ~ ",SurvFeature))
    summary(survfit(form, data = bladder_data), times = 12)
  })
  
  # Table for 1-year survival time estimate
  output$oneYearTable <- renderTable({
    SurvFeature = input$strata_col
    form <- as.formula(paste0("Surv(clinical_os_JD_num,NewRecurrenceOutcomeTable_num) ~ ",SurvFeature))
    survfit(form, data = bladder_data) %>% 
      tbl_survfit(
        times = 12,
        label_header = "1-Year Recurrence Free Survival (95% CI)"
      ) %>% 
      as_tibble()
  })
  
  # Estimating median survival time (weeks)
  output$medianEstimate <- renderPrint({
    SurvFeature = input$strata_col
    form <- as.formula(paste0("Surv(clinical_os_JD_num,NewRecurrenceOutcomeTable_num) ~ ",SurvFeature))
    survfit(form, data = bladder_data)
  })
  
  # Table for medium survival time estimate
  output$medianTable <- renderTable({
    SurvFeature = input$strata_col
    form <- as.formula(paste0("Surv(clinical_os_JD_num,NewRecurrenceOutcomeTable_num) ~ ",SurvFeature))
    survfit(form, data = bladder_data) %>% 
      tbl_survfit(
        probs = 0.5,
        label_header = "Median Recurrence Free Survival (95% CI)"
      ) %>% 
      as_tibble()
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
