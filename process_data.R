library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(googlesheets4)
library(openxlsx)
source("R/functions/labeler_function.R")
source("R/functions/cleaning_aggregation_functions.R")
# Import Data -------------------------------------------------------------

### Direct Observation
path_do <- "input/data_downlaods/latest_data/REACH Direct Observation Form.xlsx"
path_do_cto_tool <- "input/cto_tool/DO.xlsx"
excel_sheets(path_do)

do_main <- read_excel(path_do, sheet = "data", guess_max = 100000)
do_Photos_Defects_rep <- read_excel(path_do, sheet = "Photos_Defects_rep", guess_max = 100000) %>% select(-Surveyor_Name_Defects_Witness)
do_Photo_Hand_Washing <- read_excel(path_do, sheet = "Photo_Hand_Washing" , guess_max = 100000 ) %>% select(-Surveyor_Name_Hand_Washing)
do_cases_brought_surveyors <- read_excel(path_do, sheet = "cases_brought_surveyors" , guess_max = 100000) %>% select(-Surveyor_Name_Cases_Brough_Surveyours)
do_Hh_Headed_Woman_interview <- read_excel(path_do, sheet = "Hh_Headed_Woman_interview" , guess_max = 100000) %>% select(-Surveyor_Name_Headed_Woman)
do_From1photo <- read_excel(path_do, sheet = "From1photo", col_types = "text") %>% select(-Surveyor_Name_Form_1)
do_from2phto <- read_excel(path_do, sheet = "from2phto" , guess_max = 100000) %>% select(-Surveyor_Name_Form_2)
do_form3photo <- read_excel(path_do, sheet = "form3photo" , guess_max = 100000) %>% select(-Surveyor_Name_Form_3)
do_form4aphoto <- read_excel(path_do, sheet = "form4aphoto" , guess_max = 100000) %>% select(-Surveyor_Name_Form_4a)
do_form4bphoto <- read_excel(path_do, sheet = "form4bphoto" ,col_types = "text") %>% select(-Surveyor_Name_Form_4b)
do_from5photo <- read_excel(path_do, sheet = "from5photo", col_types = "text") %>% select(-Surveyor_Name_Form_5)

### Remove Extra columns / anonymize
# D0 Main
do_main_extra_cols <- c(
  "Deviceid",
  "Subscriberid",
  "Simid",
  "Devicephonenum",
  "Username",
  "aa_1",
  "Surveyor_Name",
  "KII_Name",
  "Phone_Number",
  "Translator_Name",
  "Translation_status",
  "Name_of_the_reviewer",
  "Data_Clerk_Status",
  "Data_Clerk",
  "review_comments",
  "review_status",
  "review_quality",
  "QA_status",
  "Qaed_by",
  "Data_Clerk_QA",
  "If_rejected_reason_for_the_rejection.",
  "review_corrections"
)

do_main <- do_main %>% select(-any_of(do_main_extra_cols))

### Format Dates
do_main$SubmissionDate <- as.Date(do_main$SubmissionDate, origin = "1899-12-30")
do_main <- do_main %>% mutate(
  Starttime = substr(Starttime, 5, 24),
  Endtime = substr(Endtime, 5, 24),
  Startdate =  as.Date(Starttime, "%b %d %Y %H:%M:%S"),
  Enddate = as.Date(Endtime, "%b %d %Y %H:%M:%S"),
  .after = Endtime )


### Pre-distribution Form1
path_form1 <- "input/data_downlaods/latest_data/REACH PRE-DISTRIBUTION FORM 1.xlsx"
path_form1_cto_tool <- "input/cto_tool/Form1.xlsx"
excel_sheets(path_form1)
form1_main <- read_excel(path_form1, sheet = "data" , guess_max = 100000)
form1_beneficiary_list_photos <- read_excel(path_form1, sheet = "beneficiary_list_photos" , guess_max = 100000) %>% select(-Surveyor_Name_List_Photo)

### Remove Extra columns / anonymize

form1_extra_cols <- c(
  'Deviceid',
  'Subscriberid',
  'Simid',
  'Devicephonenum',
  'Username',
  'aa_1',
  'Geopoint1-Latitude',
  'Geopoint1-Longitude',
  'Geopoint1-Altitude',
  'Geopoint1-Accuracy',
  'Surveyor_Name',
  'name_respondent',
  'phone_number_respondent',
  'In_which_lanuage_have_you_recorded_your_audios_other',
  'Geopoint2-Latitude',
  'Geopoint2-Longitude',
  'Geopoint2-Altitude',
  'Geopoint2-Accuracy',
  'QA_status',
  'Qaed_by',
  'Data_Clerk_QA',
  'If_rejected_reason_for_the_rejection.',
  'Translation_status',
  'Translator_Name',
  'Name_of_the_reviewer',
  'Data_Clerk_Status',
  'Data_Clerk',
  'review_status',
  'review_quality',
  'review_comments',
  'review_corrections'
)


form1_main <- form1_main %>% select(-any_of(form1_extra_cols))

### Format Dates
form1_main$SubmissionDate <- as.Date(form1_main$SubmissionDate, origin = "1899-12-30")
form1_main <- form1_main %>% mutate(
  Starttime = substr(Starttime, 5, 24),
  Endtime = substr(Endtime, 5, 24),
  Startdate =  as.Date(Starttime, "%b %d %Y %H:%M:%S"),
  Enddate = as.Date(Endtime, "%b %d %Y %H:%M:%S"),
  .after = Endtime )


### Pre-distribution From2
path_form2 <- "input/data_downlaods/latest_data/REACH PRE-DISTRIBUTION FORM 2.xlsx"
path_form2_cto_tool <- "input/cto_tool/Form2.xlsx"
excel_sheets(path_form2)
form2_main <- read_excel(path_form2, sheet = "data" , guess_max = 100000)
form2_Benificiary_Door_To_Door <- read_excel(path_form2, sheet = "Benificiary_Door_To_Door" , guess_max = 100000) %>% select(-c(Surveyor_Name_HH_Door_To_Door, Head_Of_Household_Name, Line_Number___Serial_Number_Of_RespondentS_Name_On_The_List,Contact_Number, What_Is_The_Serial_Number_Of_This_Other_Person_On_The_List_From_Right_Side_Column_On_Form_1, What_Is_The_Name_Of_The_Other_Person ))
form2_HH_Not_found <- read_excel(path_form2, sheet = "HH_Not_found" , guess_max = 100000) %>% select(-c(Surveyor_Name_HH_Not_Found, Name_Of_Respondent, Phone_Number_Of_Respondent, )) 
form2_HH_Not_found_One_One <- read_excel(path_form2, sheet = "HH_Not_found_One_One" , guess_max = 100000) %>% select(-c(Name_Of_Person_Not_Found, Serial_Number_Person_Not_Found, ))
form2_HH_Not_found_One_One$PARENT_KEY <- gsub("/.*","",form2_HH_Not_found_One_One$PARENT_KEY)

### Remove Extra columns / anonymize

form2_extra_cols <- c(
  'Deviceid',
  'Subscriberid',
  'Simid',
  'Devicephonenum',
  'Username',
  'aa_1',
  'Geopoint1-Latitude',
  'Geopoint1-Longitude',
  'Geopoint1-Altitude',
  'Geopoint1-Accuracy',
  'Surveyor_Name',
  'Geopoint2-Latitude',
  'Geopoint2-Longitude',
  'Geopoint2-Altitude',
  'Geopoint2-Accuracy',
  'QA_status',
  'Qaed_by',
  'Data_Clerk_QA',
  'If_rejected_reason_for_the_rejection.',
  'Translation_status',
  'Translator_Name',
  'Name_of_the_reviewer',
  'Data_Clerk_Status',
  'Data_Clerk',
  'review_status',
  'review_quality',
  'review_comments',
  'review_corrections'
)

form2_main <- form2_main %>% select(-any_of(form2_extra_cols))

### Format Dates
form2_main$SubmissionDate <- as.Date(form2_main$SubmissionDate, origin = "1899-12-30")
form2_main <- form2_main %>% mutate(
  Starttime = substr(Starttime, 5, 24),
  Endtime = substr(Endtime, 5, 24),
  Startdate =  as.Date(Starttime, "%b %d %Y %H:%M:%S"),
  Enddate = as.Date(Endtime, "%b %d %Y %H:%M:%S"),
  .after = Endtime )


# Clean, aggregate and merge Data_Clerk entries ---------------------------

### Direct Observation

### Direct Observation form1photo repeatsheet
# Typecast Data_Clerk Cols and handle strings in numeric var
do_From1photo <- do_From1photo %>% mutate_at(vars(ends_with("_N")), type_cast)

# Aggregate repeat sheet Data_Clerk cols 
do_From1photo_agg <- do_From1photo %>% group_by(PARENT_KEY) %>% 
  summarise(
    Picture_Clear_Form_1 = Mode(Picture_Clear_Form_1_N),
    Picture_Clear_Form_1_No_Why = Mode(Picture_Clear_Form_1_No_Why_N),
    Form1_Female_Headed = custom_sum(Form1_Female_Headed_N),
    Form1_A_Disability = custom_sum(Form1_A_Disability_N),
    Form1_65_And_Above = custom_sum(Form1_65_And_Above_N),
    New_Idp_Hh = custom_sum(New_Idp_Hh_N),
    New_Returnee_Hh = custom_sum(New_Returnee_Hh_N),
    Economic_Migrants_Hh = custom_sum(Economic_Migrants_Hh_N),
    New_Kuchis_Hh = custom_sum(New_Kuchis_Hh_N),
    Covid1_Migrant_Hh = custom_sum(Covid1_Migrant_Hh_N),
  )

# Convert Aggregated data to cleaning log
do_From1photo_log <- do_From1photo_agg %>% 
  pivot_longer(cols = Form1_Female_Headed:Covid1_Migrant_Hh,
               names_to = "question",
               values_to = "new_value") %>% mutate(
                 old_value = NA_character_,
                 new_value = as.character(new_value)
               ) %>% select(question, old_value, new_value, uuid = PARENT_KEY ) %>% 
  filter(!is.na(new_value))


# Bring Values to main sheet
for (rowi in 1:nrow(do_From1photo_log)){
  uuid_i <- do_From1photo_log$uuid[rowi]
  var_i <- do_From1photo_log$question[rowi]
  old_i <- do_From1photo_log$old_value[rowi]
  new_i <- do_From1photo_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  do_main[do_main$KEY == uuid_i, var_i] <- new_i
}

### Direct Observation form4bphoto repeatsheet
# Typecast Data_Clerk Cols and handle strings in numeric var
do_form4bphoto <- do_form4bphoto %>% mutate_at(vars(ends_with("_N")), type_cast)

# Aggregate repeat sheet Data_Clerk cols 
do_form4bphoto_agg <- do_form4bphoto %>% group_by(PARENT_KEY) %>% 
  summarise(
    Picture_Clear_Of_The_Entire_Form_4B = Mode(Picture_Clear_Of_The_Entire_Form_4B_N),
    Picture_Clear_Of_4B_No_Why          = Mode(Picture_Clear_Of_4B_No_Why_N),
    Rice_Kg_Per_Hh_Form_4B              = min(Rice_Kg_Per_Hh_Form_4B_N, na.rm = T),
    Flour_Kg_Per_Hh_Form_4B             = min(Flour_Kg_Per_Hh_Form_4B_N, na.rm = T),
    Quantity_Oil_Per_Hh_Form_4B         = min(Quantity_Oil_Per_Hh_Form_4B_N, na.rm = T),
    Beans_Kg_Per_Hh_Form_4B             = min(Beans_Kg_Per_Hh_Form_4B_N, na.rm = T),
    Kg_Hh_Per_Hh_Form_4B                = min(Kg_Hh_Per_Hh_Form_4B_N, na.rm = T),
    Soap_Pieces_Per_Hh_Form_4B          = min(Soap_Pieces_Per_Hh_Form_4B_N, na.rm = T),
    
  ) %>% suppressWarnings() %>% mutate_if(is.numeric, list(~na_if(.,Inf))) 

# Convert Aggregated data to cleaning log
do_form4bphoto_log <- do_form4bphoto_agg %>% 
  pivot_longer(cols = Picture_Clear_Of_The_Entire_Form_4B:Soap_Pieces_Per_Hh_Form_4B,
               names_to = "question",
               values_to = "new_value") %>% mutate(
                 old_value = NA_character_,
                 new_value = as.character(new_value)
               ) %>% select(question, old_value, new_value, uuid = PARENT_KEY ) %>% 
  filter(!is.na(new_value))

# Bring Values to main sheet
for (rowi in 1:nrow(do_form4bphoto_log)){
  uuid_i <- do_form4bphoto_log$uuid[rowi]
  var_i <- do_form4bphoto_log$question[rowi]
  old_i <- do_form4bphoto_log$old_value[rowi]
  new_i <- do_form4bphoto_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  do_main[do_main$KEY == uuid_i, var_i] <- new_i
}

### Direct Observation from5photo repeatsheet
# Typecast Data_Clerk Cols and handle strings in numeric var
do_from5photo <- do_from5photo %>% mutate_at(vars(ends_with("_N")), type_cast)

# Aggregate repeat sheet Data_Clerk cols 
do_from5photo_agg <- do_from5photo %>% group_by(PARENT_KEY) %>% 
  summarise(
    Picture_Clear_Copy_Form_5A_Or_5B        = Mode(Picture_Clear_Copy_Form_5A_Or_5B_N),
    Picture_Clear_Copy_Form_5A_Or_5B_No_Why = Mode(Picture_Clear_Copy_Form_5A_Or_5B_No_Why_N),
    Rice_Kg_Per_Hh_Form_5                   = min(Rice_Kg_Per_Hh_Form_5_N, na.rm = T),
    Flour_Kg_Per_Hh_Form_5                  = min(Flour_Kg_Per_Hh_Form_5_N, na.rm = T),
    Quantity_Oil_Litres_Per_Hh_Form_5       = min(Quantity_Oil_Litres_Per_Hh_Form_5_N, na.rm = T),
    Beans_Kg_Per_Hh_Form_5                  = min(Beans_Kg_Per_Hh_Form_5_N, na.rm = T),
    Kg_Per_Hh_Per_Form_5                    = min(Kg_Per_Hh_Per_Form_5_N, na.rm = T),
    Soap_Bar_Per_Hh_Per_Form_5              = min(Soap_Bar_Per_Hh_Per_Form_5_N, na.rm = T),
    Money_Per_HH_Per_Form_5                 = min(Money_Per_HH_Per_Form_5_N, na.rm = T),
    Other_Amount_Integer_Per_Form_5         = min(Other_Amount_Integer_Per_Form_5_N, na.rm = T)
  )  %>% suppressWarnings() %>% mutate_if(is.numeric, list(~na_if(.,Inf))) 

# Convert Aggregated data to cleaning log
do_from5photo_log <- do_from5photo_agg %>% 
  pivot_longer(cols = Picture_Clear_Copy_Form_5A_Or_5B:Other_Amount_Integer_Per_Form_5,
               names_to = "question",
               values_to = "new_value") %>% mutate(
                 old_value = NA_character_,
                 new_value = as.character(new_value)
               ) %>% select(question, old_value, new_value, uuid = PARENT_KEY ) %>% 
  filter(!is.na(new_value))

# Bring Values to main sheet
for (rowi in 1:nrow(do_from5photo_log)){
  uuid_i <- do_from5photo_log$uuid[rowi]
  var_i <- do_from5photo_log$question[rowi]
  old_i <- do_from5photo_log$old_value[rowi]
  new_i <- do_from5photo_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  do_main[do_main$KEY == uuid_i, var_i] <- new_i
}

# Pre-distribution form1

# Typecast Data_Clerk Cols and handle strings in numeric var
form1_beneficiary_list_photos <- form1_beneficiary_list_photos %>% mutate_at(vars(ends_with("_N")), type_cast)

# Aggregate repeat sheet Data_Clerk cols 
form1_beneficiary_list_photos_agg <- form1_beneficiary_list_photos %>% group_by(PARENT_KEY) %>% 
  summarise(
    Is_The_Picture_A_Clear_Copy_Of_The_Entire_Form    = Mode(Is_The_Picture_A_Clear_Copy_Of_The_Entire_Form_N),
    Reasons_For_Why_The_Copy_Of_The_Form_Is_Not_Clear = Mode(`_Reasons_For_Why_The_Copy_Of_The_Form_Is_Not_Clear_N`),
    Number_eligible_HH_form1                         = custom_sum(Number_eligible_HH_form1_N),
    Number_ineligible_HH_form1                        = custom_sum(Number_ineligible_HH_form1_N),
    Number_Of_Female_Headed_Household                 = custom_sum(Number_Of_Female_Headed_Household_N),
    Number_Of_HH_Headed_By_A_Person_With_A_Disability = custom_sum(Number_Of_HH_Headed_By_A_Person_With_A_Disability_N),
    Number_Of_HH_Headed_By_Elderly                    = custom_sum(Number_Of_HH_Headed_By_Elderly_N),
    Number_Of_HH_Entered_Under_Details_To_Be_Added_For_HHs_Not_Shown_Above_From_The_Time_Of_Ccap_Mobilization_Of_This_Community = custom_sum(Number_Of_HH_Entered_Under_Details_To_Be_Added_For_HHs_Not_Shown_Above_From_The_Time_Of_Ccap_Mobilization_Of_This_Community_N),
    New_Idp_HH            = custom_sum(New_Idp_HH_N),
    New_Returnee_HH      = custom_sum(New_Returnee_HH_N),
    Economic_Migrants_HH = custom_sum(Economic_Migrants_HH_N),
    New_Kuchis_HH        = custom_sum(New_Kuchis_HH_N),
    Covid_19_Migrant_HH  = custom_sum(Covid_19_Migrant_HH_N),
    
  ) 

# Convert Aggregated data to cleaning log
form1_beneficiary_list_photos_log <- form1_beneficiary_list_photos_agg %>% 
  pivot_longer(cols = Is_The_Picture_A_Clear_Copy_Of_The_Entire_Form:Covid_19_Migrant_HH,
               names_to = "question",
               values_to = "new_value") %>% mutate(
                 old_value = NA_character_,
                 new_value = as.character(new_value)
               ) %>% select(question, old_value, new_value, uuid = PARENT_KEY ) %>% 
  filter(!is.na(new_value))


# Bring Values to main sheet
for (rowi in 1:nrow(form1_beneficiary_list_photos_log)){
  uuid_i <- form1_beneficiary_list_photos_log$uuid[rowi]
  var_i <- form1_beneficiary_list_photos_log$question[rowi]
  old_i <- form1_beneficiary_list_photos_log$old_value[rowi]
  new_i <- form1_beneficiary_list_photos_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  form1_main[form1_main$KEY == uuid_i, var_i] <- new_i
}



# Label Data --------------------------------------------------------------

# Direct Observation
do_main <- labeler(data = do_main,
                   tool = path_do_cto_tool,
                   survey_label = "label:English",
                   choice_lable = "label")

do_cases_brought_surveyors <- labeler(data = do_cases_brought_surveyors,
                                      tool = path_do_cto_tool,
                                      survey_label = "label:English",
                                      choice_lable = "label")

do_Hh_Headed_Woman_interview <- labeler(data = do_Hh_Headed_Woman_interview,
                                        tool = path_do_cto_tool,
                                        survey_label = "label:English",
                                        choice_lable = "label")

# pre-distribution form1
form1_main <- labeler(data = form1_main,
                      tool = path_form1_cto_tool,
                      survey_label = "label:English",
                      choice_lable = "label")

# pre-distribution form2
form2_main <- labeler(data = form2_main,
                      tool = path_form2_cto_tool,
                      survey_label = "label:English",
                      choice_lable = "label")

form2_Benificiary_Door_To_Door <- labeler(data = form2_Benificiary_Door_To_Door,
                                          tool = path_form2_cto_tool,
                                          survey_label = "label:English",
                                          choice_lable = "label")

form2_HH_Not_found <- labeler(data = form2_HH_Not_found,
                              tool = path_form2_cto_tool,
                              survey_label = "label:English",
                              choice_lable = "label")

form2_HH_Not_found_One_One <- labeler(data = form2_HH_Not_found_One_One,
                                      tool = path_form2_cto_tool,
                                      survey_label = "label:English",
                                      choice_lable = "label")

# Merge with QA Log and Reported Log --------------------------------------

gs4_deauth()
qa_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQhlF8-SoqkzJg6FSYLV2bn08nikuYhh9wQSm-fH36wKNYP3rQqCMh01HoNJ1tzrMehEF99lDFlG3F4/pub?gid=589852754&single=true&output=csv")
qa_log <- qa_log %>% select(UUID, `Form Type`, `CDC ID`,Status)

# Direct Observation
do_qa_log <- qa_log %>% 
  dplyr::filter(!is.na(UUID)) %>% 
  filter(`Form Type` == "Direct observation") %>% 
  distinct(UUID, .keep_all = T) %>% 
  mutate(
    Status = case_when(
      is.na(Status) ~ "Pending",
      TRUE ~ Status
    )
  )

# pre-distribution form1
form1_qa_log <- qa_log %>% 
  dplyr::filter(!is.na(UUID)) %>% 
  filter(`Form Type` == "Pre-distribution form 1") %>% 
  distinct(UUID, .keep_all = T) %>% 
  mutate(
    Status = case_when(
      is.na(Status) ~ "Pending",
      TRUE ~ Status
    )
  )

# pre-distribution form2
form2_qa_log <- qa_log %>% 
  dplyr::filter(!is.na(UUID)) %>% 
  filter(`Form Type` == "Pre-distribution form 2") %>% 
  distinct(UUID, .keep_all = T) %>% 
  mutate(
    Status = case_when(
      is.na(Status) ~ "Pending",
      TRUE ~ Status
    )
  )

do_main <- do_main %>% left_join(select(do_qa_log, UUID, Status), by = c("KEY" = "UUID")) 
form1_main <- form1_main %>% left_join(select(form1_qa_log, UUID, Status), by = c("KEY" = "UUID")) 
form2_main <- form2_main %>% left_join(select(form2_qa_log, UUID, Status), by = c("KEY" = "UUID")) 

### Reported
# Direct Observation
print("Reading Reported Sheet!")
cleaned_link <- "https://docs.google.com/spreadsheets/d/1Dk80Rtf19ytaqg5Mnd1zysrsqpY87eHPDZHy__B1dnc/edit#gid=1328441376"
do_reported_data <- read_sheet(cleaned_link, sheet = "DO_Reported") %>% distinct(KEY, .keep_all = T)

do_main <- do_main %>% left_join(select(do_reported_data , KEY, weekly_reporting_round ), by = "KEY")

# pre-distribution form1
form1_reported_data <- read_sheet(cleaned_link, sheet = "Form1_Reported") %>% distinct(KEY, .keep_all = T)
form1_main <- form1_main %>% left_join(select(form1_reported_data , KEY, weekly_reporting_round ), by = "KEY")


# pre-distribution form2
form2_reported_data <- read_sheet(cleaned_link, sheet = "Form2_reported") %>% distinct(KEY, .keep_all = T)
form2_main <- form2_main %>% left_join(select(form2_reported_data , KEY, weekly_reporting_round ), by = "KEY")


# Apply cleaning Log ------------------------------------------------------

do_main_cleaning_log <- read_sheet(cleaned_link, sheet = "DO_log")
form1_main_cleaning_log <- read_sheet(cleaned_link, sheet = "Form1_log")
form2_main_cleaning_log <- read_sheet(cleaned_link, sheet = "Form2_log")


for (rowi in 1:nrow(do_main_cleaning_log)){
  
  uuid_i <- do_main_cleaning_log$uuid[rowi]
  var_i <- do_main_cleaning_log$question[rowi]
  old_i <- do_main_cleaning_log$old_value[rowi]
  new_i <- do_main_cleaning_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  do_main[do_main$KEY == uuid_i, var_i] <- new_i
}

for (rowi in 1:nrow(form1_main_cleaning_log)){
  
  uuid_i <- form1_main_cleaning_log$uuid[rowi]
  var_i <- form1_main_cleaning_log$question[rowi]
  old_i <- form1_main_cleaning_log$old_value[rowi]
  new_i <- form1_main_cleaning_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  form1_main[form1_main$KEY == uuid_i, var_i] <- new_i
}

for (rowi in 1:nrow(form2_main_cleaning_log)){
  
  uuid_i <- form2_main_cleaning_log$uuid[rowi]
  var_i <- form2_main_cleaning_log$question[rowi]
  old_i <- form2_main_cleaning_log$old_value[rowi]
  new_i <- form2_main_cleaning_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  form2_main[form2_main$KEY == uuid_i, var_i] <- new_i
}

# Processed Raw Data backup -----------------------------------------------

do_list <- list(
  data = do_main,
  Photos_Defects_rep = do_Photos_Defects_rep,
  Photo_Hand_Washing = do_Photo_Hand_Washing,
  cases_brought_surveyors = do_cases_brought_surveyors,
  Hh_Headed_Woman_interview = do_Hh_Headed_Woman_interview,
  From1photo = do_From1photo,
  from2phto = do_from2phto,
  form3photo = do_form3photo,
  form4aphoto = do_form4aphoto,
  form4bphoto = do_form4bphoto,
  from5photo = do_from5photo
  
)

### Pre-distribution Form1
form1_list <- list(
  data = form1_main,
  beneficiary_list_photos = form1_beneficiary_list_photos
)


### Pre-distribution From2
form2_list <- list(
  data = form2_main,
  Benificiary_Door_To_Door = form2_Benificiary_Door_To_Door,
  HH_Not_found = form2_HH_Not_found,
  HH_Not_found_One_One = form2_HH_Not_found_One_One
)


write.xlsx(do_list, paste0("output/proccessed_raw_data/direct_observation/REACH Direct Observation Form_",today(),".xlsx" ))
write.xlsx(form1_list, paste0("output/proccessed_raw_data/pre_distribution_form1/REACH PRE-DISTRIBUTION FORM 1_",today(),".xlsx" ))
write.xlsx(form2_list, paste0("output/proccessed_raw_data/pre_distribution_form2/REACH PRE-DISTRIBUTION FORM 2_",today(),".xlsx" ))

write.xlsx(do_list, paste0("output/proccessed_raw_data/latest_data/REACH Direct Observation Form.xlsx" ))
write.xlsx(form1_list, paste0("output/proccessed_raw_data/latest_data/REACH PRE-DISTRIBUTION FORM 1.xlsx" ))
write.xlsx(form2_list, paste0("output/proccessed_raw_data/latest_data/REACH PRE-DISTRIBUTION FORM 2.xlsx" ))

# Week Specific Data ------------------------------------------------------
week = c(12,13,14,15,16)

# Direct Observation
do_main_filtered <- do_main %>% filter(Distributed_Covid19_Relief != "Nothing [end of questionnaire – surveyor to call head office]" & Status == "Approved" & weekly_reporting_round %in% week)
do_Photos_Defects_rep_filtered <- do_Photos_Defects_rep %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_Photo_Hand_Washing_filtered <- do_Photo_Hand_Washing %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_cases_brought_surveyors_filtered <- do_cases_brought_surveyors %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_Hh_Headed_Woman_interview_filtered <- do_Hh_Headed_Woman_interview %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_From1photo_filtered <- do_From1photo %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_from2phto_filtered <- do_from2phto %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_form3photo_filtered <- do_form3photo %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_form4aphoto_filtered <- do_form4aphoto %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_form4bphoto_filtered <- do_form4bphoto %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)
do_from5photo_filtered <- do_from5photo %>%  filter(PARENT_KEY %in% do_main_filtered$KEY)


do_list_filtered <- list(
  data = do_main_filtered,
  Photos_Defects_rep = do_Photos_Defects_rep_filtered,
  Photo_Hand_Washing = do_Photo_Hand_Washing_filtered,
  cases_brought_surveyors = do_cases_brought_surveyors_filtered,
  Hh_Headed_Woman_interview = do_Hh_Headed_Woman_interview_filtered,
  From1photo = do_From1photo_filtered,
  from2phto = do_from2phto_filtered,
  form3photo = do_form3photo_filtered,
  form4aphoto = do_form4aphoto_filtered,
  form4bphoto = do_form4bphoto_filtered,
  from5photo = do_from5photo_filtered
  
)

### Pre-distribution Form1
form1_main_filtered <- form1_main %>% filter(Are_You_Willing_To_Be_Interviewed == "Yes" & Status == "Approved" & weekly_reporting_round %in% week)
form1_beneficiary_list_photos_filtered <- form1_beneficiary_list_photos %>%  filter(PARENT_KEY %in% form1_main_filtered$KEY)

form1_list_filtered <- list(
  data = form1_main_filtered,
  beneficiary_list_photos = form1_beneficiary_list_photos_filtered
)


### Pre-distribution From2
form2_main_filtered <- form2_main %>% filter(Status == "Approved" & weekly_reporting_round %in% week)
form2_Benificiary_Door_To_Door_filtered <- form2_Benificiary_Door_To_Door %>%  filter(Are_You_Willing_To_Be_Interviewed == "Yes" & PARENT_KEY %in% form2_main_filtered$KEY)
form2_HH_Not_found_filtered <- form2_HH_Not_found %>% filter(Do_You_Agree_To_Being_Interviewed == "Yes" & PARENT_KEY %in% form2_main_filtered$KEY)
form2_HH_Not_found_One_One_filtered <- form2_HH_Not_found_One_One %>% filter(PARENT_KEY %in% form2_main_filtered$KEY)

form2_list_filtered <- list(
  data = form2_main_filtered,
  Benificiary_Door_To_Door = form2_Benificiary_Door_To_Door_filtered,
  HH_Not_found = form2_HH_Not_found_filtered,
  HH_Not_found_One_One = form2_HH_Not_found_One_One_filtered
)


week <- "_12_to_16" # temporary, just for monthly
write.xlsx(do_list_filtered, paste0("output/week_specific/direct_observation/REACH Direct Observation Form_",today(),"_Week",week,".xlsx" ))
write.xlsx(form1_list_filtered, paste0("output/week_specific/pre_distribution_form1/REACH PRE-DISTRIBUTION FORM 1_",today(),"_Week",week,".xlsx" ))
write.xlsx(form2_list_filtered, paste0("output/week_specific/pre_distribution_form2/REACH PRE-DISTRIBUTION FORM 2_",today(),"_Week",week,".xlsx" ))

