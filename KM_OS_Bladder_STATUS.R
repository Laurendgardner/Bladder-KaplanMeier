# Install packages
install.packages("ggsurvfit")
install.packages("gtsummary")

# Load packages
library(ggsurvfit)
library(gtsummary)

# Read "bladder.tsv" file into a data frame named "bladder_data"
bladder_data <- read.table(file = "/Users/laurengardner/Projects/KP_BladderProject/bladder.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Create a new column named "NewReccurranceOutcomeTable"
bladder_data$NewReccurrenceOutcomeTable = "NA"

# Assign status code for new column
bladder_data[which(bladder_data$recurrence_outcome_JD == "Yes"), "NewReccurrenceOutcomeTable"] = 1
bladder_data[which(bladder_data$recurrence_outcome_JD == "No"), "NewReccurrenceOutcomeTable"] = 0

# Create a new column named "NewRecurrenceOutcomeTable_num" with numerical character values
bladder_data$NewRecurrenceOutcomeTable_num <- as.numeric(as.character(bladder_data$NewReccurrenceOutcomeTable))

# Create a new column named "clinical_os_JD_num" with numerical character values
bladder_data$clinical_os_JD_num <- as.numeric(as.character(bladder_data$clinical_os_JD))

# Create Kaplan-Meier plot
survfit2(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  ggsurvfit() +
  labs(
    x = "Weeks",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()

# Estimating 1-year survival (weeks)
summary(survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data), times = 52)

# Table for 1-year survival time estimate
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  tbl_survfit(
    times = 52,
    label_header = "1-year survival (95% CI)"
  )

# Estimating median survival time (weeks)
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data)

# Table for medium survival time estimate
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  tbl_survfit(
    probs = 0.5,
    label_header = "Median survival (95% CI)"
  )




