library(readxl)
library(dplyr)


cleaning_log  <- read_excel("input/DO_cleaning_log/DO_cleaning_log.xlsx")
# Direct Observation ------------------------------------------------------
get_do_week_specific <- function(week, name){
  do_main <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "data")
  do_goods <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "goods")
  do_Rondom_pakage1 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage1")
  do_Rondom_pakage2 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage2")
  do_Rondom_pakage3 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage3")
  do_Photos_Of_The_Defects_Witnessed <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Photos_Of_The_Defects_Witnessed")
  do_rep_If_Yes_Photos_All_Docume <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "rep_If_Yes_Photos_All_Docume")
  
  remove_cols <- c(
    "Deviceid",
    "Subscriberid",
    "Simid",
    "Devicephonenum",
    "Username",
    "aa_1",
    "text_audit",
    "Surveyor_Name",
    "Translator_Name",
    "In_which_lanuage_have_you_recorded_your_audios",
    "In_which_lanuage_have_you_recorded_your_audios_other",
    "QA_status",
    "Qaed_by",
    "If_rejected_reason_for_the_rejection.",
    "Translation_status",
    "Translator_Name"	,
    "Name_of_the_reviewer",
    "download_date",
    "cleaned"
    
  )
  
  do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance <- as.character(do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance)
  do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance <- as.character(do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance)
  do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance <- as.character(do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance)
  # Apply cleaning log on raw data
  for (rowi in 1:nrow(cleaning_log)){
    
    uuid_i <- cleaning_log$uuid[rowi]
    var_i <- cleaning_log$question[rowi]
    old_i <- cleaning_log$old_value[rowi]
    new_i <- cleaning_log$new_value[rowi]
    print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    # Find the variable according to the row of the cleaning log
    do_main[do_main$KEY == uuid_i, var_i] <- new_i
  }
  
  do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance <- as.numeric(do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance)
  do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance <- as.numeric(do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance)
  do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance <- as.numeric(do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance)
  
  
  do_main <- do_main %>% filter(Status != "Rejected" & weekly_reporting_round %in% week) %>% select(-all_of(remove_cols))
  do_goods <- do_goods %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  do_Rondom_pakage1 <- do_Rondom_pakage1 %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  do_Rondom_pakage2 <- do_Rondom_pakage2  %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  do_Rondom_pakage3 <- do_Rondom_pakage3  %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  do_Photos_Of_The_Defects_Witnessed <- do_Photos_Of_The_Defects_Witnessed  %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  do_rep_If_Yes_Photos_All_Docume <- do_rep_If_Yes_Photos_All_Docume  %>% filter(Status != "Rejected" & weekly_reporting_round %in% week)
  
  
  do_data_list <- list(
    data = do_main,
    goods = do_goods,
    Rondom_pakage1 = do_Rondom_pakage1,
    Rondom_pakage2 = do_Rondom_pakage2,
    Rondom_pakage3 = do_Rondom_pakage3,
    Photos_Of_The_Defects_Witnessed = do_Photos_Of_The_Defects_Witnessed,
    rep_If_Yes_Photos_All_Docume = do_rep_If_Yes_Photos_All_Docume
  )
  
  write.xlsx(do_data_list, paste0("output/week_specific/Direct_observation/direct_observation_week", name,"_", today(), ".xlsx"))
  print(paste0("Direct Observation Week ", week, " Data is saved in output/week_specific/Direct_observation/ Folder "))
  
}



# Pre-Distribution From1 --------------------------------------------------

get_form1_week_specific <- function(week, name){
  form1 <- read_excel("output/latest_data/pre_distribution_form1/pre_distribution_form1.xlsx")
  
  remove_cols <- c(
    "aa_1",
    "text_audit",
    "Surveyor_Name",
    "Geopoint1-Latitude",
    "Geopoint1-Longitude",
    "Geopoint1-Altitude",
    "Geopoint1-Accuracy",
    "name_respondent",
    "phone_number_respondent",
    "weekly_reporting_date",
    "download_date",
    "cleaned"

  )
  
  form1 <- form1 %>% filter(Status != "Rejected" & weekly_reporting_round %in% week) %>% select(-all_of(remove_cols))
  
  write.xlsx(form1, paste0("output/week_specific/Pre_distribution_form1/pre_distribution_form1_week", name,"_", today(), ".xlsx"))
  print(paste0("Pre-Distribution From1 ", week, " Data is saved in output/week_specific/Pre_distribution_form1/ Folder "))
}


# Pre-Distribution Form2 --------------------------------------------------

get_form2_week_specific <- function(week, name){
  form2 <- read_excel("output/latest_data/pre_distribution_form2/Pre_Distribution_form2.xlsx")
  
  remove_cols <- c(
    "aa_1",
    "text_audit",
    "Surveyor_Name",
    "Geopoint1-Latitude",
    "Geopoint1-Longitude",
    "Geopoint1-Altitude",
    "Geopoint1-Accuracy",
    "Head_Of_Household_Name",
    "Contact_Number",
    "download_date",
    "cleaned"
  )
  
  form2 <- form2 %>% filter(Status != "Rejected" & weekly_reporting_round %in% week) %>% select(-all_of(remove_cols))
  
  write.xlsx(form2, paste0("output/week_specific/Pre_distribution_form2/pre_distribution_form2_week", name,"_", today(), ".xlsx"))
  print(paste0("Pre-Distribution From1 ", week, " Data is saved in output/week_specific/Pre_distribution_form2/ Folder "))
}




# Direct Observation Monthly Data ------------------------------------------------------
get_do_month_specific <- function(month_num, uuids, cleaning_log){
  do_main <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "data")
  do_goods <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "goods")
  do_Rondom_pakage1 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage1")
  do_Rondom_pakage2 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage2")
  do_Rondom_pakage3 <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Rondom_pakage3")
  do_Photos_Of_The_Defects_Witnessed <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "Photos_Of_The_Defects_Witnessed")
  do_rep_If_Yes_Photos_All_Docume <- read_excel("output/latest_data/direct_observation/direct_observation.xlsx", sheet = "rep_If_Yes_Photos_All_Docume")
  
  remove_cols <- c(
    "Deviceid",
    "Subscriberid",
    "Simid",
    "Devicephonenum",
    "Username",
    "aa_1",
    "text_audit",
    "Surveyor_Name",
    "Translator_Name",
    "In_which_lanuage_have_you_recorded_your_audios",
    "In_which_lanuage_have_you_recorded_your_audios_other",
    "QA_status",
    "Qaed_by",
    "If_rejected_reason_for_the_rejection.",
    "Translation_status",
    "Translator_Name"	,
    "Name_of_the_reviewer",
    "download_date",
    "cleaned"
    
  )
  
  
  do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance <- as.character(do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance)
  do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance <- as.character(do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance)
  do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance <- as.character(do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance)
  # Apply cleaning log on raw data
  for (rowi in 1:nrow(cleaning_log)){
    
    uuid_i <- cleaning_log$uuid[rowi]
    var_i <- cleaning_log$question[rowi]
    old_i <- cleaning_log$old_value[rowi]
    new_i <- cleaning_log$new_value[rowi]
    print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    # Find the variable according to the row of the cleaning log
    do_main[do_main$KEY == uuid_i, var_i] <- new_i
  }
  
  do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance <- as.numeric(do_main$Femaleheaded_Households_Eligible_Households_Received_Assistance)
  do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance <- as.numeric(do_main$Elderly_Headed_Households_Eligible_Households_Received_Assistance)
  do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance <- as.numeric(do_main$Households_With_Persons_With_Disabilities_Eligible_Households_Received_Assistance)
  
  
  do_main <- do_main %>% filter(KEY %in% uuids$KEY) %>% select(-all_of(remove_cols))
  do_goods <- do_goods %>% filter(PARENT_KEY %in% uuids$KEY)
  do_Rondom_pakage1 <- do_Rondom_pakage1 %>% filter(PARENT_KEY %in% uuids$KEY)
  do_Rondom_pakage2 <- do_Rondom_pakage2  %>% filter(PARENT_KEY %in% uuids$KEY)
  do_Rondom_pakage3 <- do_Rondom_pakage3  %>% filter(PARENT_KEY %in% uuids$KEY)
  do_Photos_Of_The_Defects_Witnessed <- do_Photos_Of_The_Defects_Witnessed  %>% filter(PARENT_KEY %in% uuids$KEY)
  do_rep_If_Yes_Photos_All_Docume <- do_rep_If_Yes_Photos_All_Docume  %>% filter(PARENT_KEY %in% uuids$KEY)
  
  
  do_data_list <- list(
    data = do_main,
    goods = do_goods,
    Rondom_pakage1 = do_Rondom_pakage1,
    Rondom_pakage2 = do_Rondom_pakage2,
    Rondom_pakage3 = do_Rondom_pakage3,
    Photos_Of_The_Defects_Witnessed = do_Photos_Of_The_Defects_Witnessed,
    rep_If_Yes_Photos_All_Docume = do_rep_If_Yes_Photos_All_Docume
  )
  
  write.xlsx(do_data_list, paste0("output/month_specific/month1/direct_observation_month", month_num,"_", today(), ".xlsx"))
  print(paste0("Direct Observation Month ", month_num, " Data is saved in output/week_specific/Direct_observation/ Folder "))
  
}



