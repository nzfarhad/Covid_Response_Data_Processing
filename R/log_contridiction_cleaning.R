library(dplyr)
library(readxl)



# Data -------------------------------------------------------------
clean_data_path <- "input/clean_dasta_for_analysis/week14/New folder2/REACH PRE-DISTRIBUTION FORM 2_2021-05-01_Week14.xlsx"
raw_data_path <- "output/week_specific/pre_distribution_form2/REACH PRE-DISTRIBUTION FORM 2_2021-05-01_Week14.xlsx"

clean_data <- read_excel(clean_data_path, na = c("", "NA"))
raw_data <- read_excel(raw_data_path , na = c("", "NA")  )

raw_data <- raw_data %>% filter(KEY %in% clean_data$KEY)
raw_data <- raw_data %>%  select(any_of(names(clean_data)))

clean_data <- clean_data %>%  select(any_of(names(raw_data)))
# check <- log_recodes(rawDF = raw_data, cleanedDF = clean_data, uuid_ = "KEY")


question <- vector()
old_value <- vector()
new_value <- vector()
uuid <- vector()
for (i in 1:length(clean_data)) {
  for (j in clean_data$KEY) {
    test <-  clean_data[clean_data$KEY == j,i] %in% raw_data[raw_data$KEY == j,i]
    
    if(isFALSE(test)){
      
      value_raw <- raw_data[raw_data$KEY == j,i]
      value_clean <- clean_data[clean_data$KEY == j,i]
      
      # append values to vectors
      question <- c(question,names(clean_data[i]))
      old_value <- c(old_value, as.character(value_raw))
      new_value <- c(new_value, as.character(value_clean))
      uuid <- c(uuid, j)
    }
}}
my_log <- data.frame(question, old_value, new_value, uuid)




openxlsx::write.xlsx(my_log, "Data/temp/From2_week14_log.xlsx")




