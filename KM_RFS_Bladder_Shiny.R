# Load required libraries
library(shiny)
library(ggsurvfit)
library(gtsummary)
library(survival)

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Kaplan-Meier Survival Analysis"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: File upload control
      fileInput("file1", "Choose TSV File",
                accept = c(
                  "text/tsv",
                  "text/tab-separated-values,text/plain",
                  ".tsv")),
      hr()
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Kaplan-Meier Plot", plotOutput("kmPlot")),
        tabPanel("1-Year Recurrence Free Survival (95% CI)", tableOutput("oneYearSummary")),
        tabPanel("Median Recurrence Free Survival (95% CI)", tableOutput("medianSummary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to read the uploaded file
  bladder_data <- reactive({
    req(input$file1)
    df <- read.table(file = input$file1$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    
    # Create a new column named "NewReccurrenceOutcomeTable"
    df$NewReccurrenceOutcomeTable <- "NA"
    
    # Assign status code for new column
    df[which(df$recurrence_outcome_JD == "Yes"), "NewReccurrenceOutcomeTable"] <- 1
    df[which(df$recurrence_outcome_JD == "No"), "NewReccurrenceOutcomeTable"] <- 0
    
    # Create a new column named "NewRecurrenceOutcomeTable_num" with numerical character values
    df$NewRecurrenceOutcomeTable_num <- as.numeric(as.character(df$NewReccurrenceOutcomeTable))
    
    return(df)
  })
  
  # Kaplan-Meier plot
  output$kmPlot <- renderPlot({
    req(bladder_data())
    surv_fit <- survfit(Surv(clinical_rfs_JD, NewRecurrenceOutcomeTable_num) ~ 1, data = bladder_data())
    ggsurvfit(surv_fit) +
      labs(
        x = "Months",
        y = "Recurrence Free Survival Probability"
      ) +
      add_confidence_interval() +
      add_risktable()
  })
  
  # Summary for 1-year recurrence free survival estimate
  output$oneYearSummary <- renderTable({
    req(bladder_data())
    surv_fit <- survfit(Surv(clinical_rfs_JD, NewRecurrenceOutcomeTable_num) ~ 1, data = bladder_data())
    tbl <- tbl_survfit(surv_fit, times = 12, label_header = "1-Year Recurrence Free Survival (95% CI)")
    as.data.frame(tbl$table_body)
  })
  
  # Summary for median recurrence free survival estimate
  output$medianSummary <- renderTable({
    req(bladder_data())
    surv_fit <- survfit(Surv(clinical_rfs_JD, NewRecurrenceOutcomeTable_num) ~ 1, data = bladder_data())
    tbl <- tbl_survfit(surv_fit, probs = 0.5, label_header = "Median Recurrence Free Survival (95% CI)")
    as.data.frame(tbl$table_body)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
