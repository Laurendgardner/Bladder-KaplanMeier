#install packages
install.packages("ggsurvfit")
install.packages("gtsummary")

#load packages
library(ggsurvfit)
library(gtsummary)

#read "bladder.tsv" file into a data frame named "bladder_data"
bladder_data <- read.table(file = "/Users/laurengardner/Projects/KP_BladderProject/bladder.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

#create a new column named "NewReccurranceOutcomeTable"
bladder_data$NewReccurrenceOutcomeTable = "NA"

#assign status code for new column
bladder_data[which(bladder_data$recurrence_outcome_JD == "Yes"), "NewReccurrenceOutcomeTable"] = 1
bladder_data[which(bladder_data$recurrence_outcome_JD == "No"), "NewReccurrenceOutcomeTable"] = 0

#create a new column named "NewRecurrenceOutcomeTable_num" with numerical character values
bladder_data$NewRecurrenceOutcomeTable_num <- as.numeric(as.character(bladder_data$NewReccurrenceOutcomeTable))

#create a new column named "clinical_os_JD_num" with numerical character values
bladder_data$clinical_os_JD_num <- as.numeric(as.character(bladder_data$clinical_os_JD))

#create Kaplan-Meier plot
survfit2(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  ggsurvfit() +
  labs(
    x = "Weeks",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()

#estimating 1-year survival (weeks)
summary(survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data), times = 52)

#table for 1-year survival time estimate
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  tbl_survfit(
    times = 52,
    label_header = "1-year survival (95% CI)"
  )

#estimating median survival time (weeks)
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data)

#table for medium survival time estimate
survfit(Surv(clinical_os_JD_num, NewRecurrenceOutcomeTable_num) ~ tstage_c_JD, data = bladder_data) %>% 
  tbl_survfit(
    probs = 0.5,
    label_header = "Median survival (95% CI)"
  )




