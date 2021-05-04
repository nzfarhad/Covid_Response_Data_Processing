library(dplyr)

# Functions

# Direct Observation ------------------------------------------------------
do_translation_check = function(direct_observation){
  
  direct_observation <- direct_observation %>% mutate(
    # If_No_Please_Explain_Any_Reasons_For_Staff_Not_Appearing_On_Time_And_Any_Resulting_Issues
    If_No_Please_Explain_Any_Reasons_For_Staff_Not_Appearing_On_Time_And_Any_Resulting_Issues_Translation_count = case_when(
      !is.na(If_No_Please_Explain_Any_Reasons_For_Staff_Not_Appearing_On_Time_And_Any_Resulting_Issues) &
        (If_No_Please_Explain_Any_Reasons_For_Staff_Not_Appearing_On_Time_And_Any_Resulting_Issues_Translation == "translation" |
           is.na(If_No_Please_Explain_Any_Reasons_For_Staff_Not_Appearing_On_Time_And_Any_Resulting_Issues_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Was_Information_For_Lodging_A_Grievance_Provided_Elsewherein_A_Different_Way_In_The_Community
    Was_Information_For_Lodging_A_Grievance_Provided_Elsewherein_A_Different_Way_In_The_Community_Translation_count = case_when(
      !is.na(Was_Information_For_Lodging_A_Grievance_Provided_Elsewherein_A_Different_Way_In_The_Community) &
        (Was_Information_For_Lodging_A_Grievance_Provided_Elsewherein_A_Different_Way_In_The_Community_Translation == "translation" |
           is.na(Was_Information_For_Lodging_A_Grievance_Provided_Elsewherein_A_Different_Way_In_The_Community_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Grievance_Contact_Information_Was_Announced_Over_Loudspeakers__
    Grievance_Contact_Information_Was_Announced_Over_Loudspeakers_Tranlsation_count = case_when(
      !is.na(Grievance_Contact_Information_Was_Announced_Over_Loudspeakers__) &
        (Grievance_Contact_Information_Was_Announced_Over_Loudspeakers_Tranlsation == "translation" |
           is.na(Grievance_Contact_Information_Was_Announced_Over_Loudspeakers_Tranlsation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Grievance_Contact_Information_Was_Passed_Out_In_Brochures_
    Grievance_Contact_Information_Was_Passed_Out_In_Brochures_Translation_count = case_when(
      !is.na(Grievance_Contact_Information_Was_Passed_Out_In_Brochures_) &
        (Grievance_Contact_Information_Was_Passed_Out_In_Brochures_Translation == "translation" |
           is.na(Grievance_Contact_Information_Was_Passed_Out_In_Brochures_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_other
    If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_other_Transilation_count = case_when(
      !is.na(If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_other) &
        (If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_other_Transilation == "translation" |
           is.na(If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_other_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Item_And_Quantity_Of_Random_Package_1_other
    Item_And_Quantity_Of_Random_Package_1_other_Transilaition_count = case_when(
      !is.na(Item_And_Quantity_Of_Random_Package_1_other) &
        (Item_And_Quantity_Of_Random_Package_1_other_Transilaition == "translation" |
           is.na(Item_And_Quantity_Of_Random_Package_1_other_Transilaition)) ~ 1,
      TRUE ~ 0
    ),
    
    # Item_And_Quantity_Of_Random_Package_2_Other
    Item_And_Quantity_Of_Random_Package_2_Other_Transilation_count = case_when(
      !is.na(Item_And_Quantity_Of_Random_Package_2_Other) &
        (Item_And_Quantity_Of_Random_Package_2_Other_Transilation == "translation" |
           is.na(Item_And_Quantity_Of_Random_Package_2_Other_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Item_and_quantity_of_random_packageother
    Item_and_quantity_of_random_packageother_Transilation_count = case_when(
      !is.na(Item_and_quantity_of_random_packageother) &
        (Item_and_quantity_of_random_packageother_Transilation == "translation" |
           is.na(Item_and_quantity_of_random_packageother_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Specify_The_Defects_You_Witnessed_Other_Please_Specify
    Specify_The_Defects_You_Witnessed_Other_Please_Specify_translation_count = case_when(
      !is.na(Specify_The_Defects_You_Witnessed_Other_Please_Specify) &
        (Specify_The_Defects_You_Witnessed_Other_Please_Specify_translation == "translation" |
           is.na(Specify_The_Defects_You_Witnessed_Other_Please_Specify_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Please_Explain_In_Detail_What_You_Observed
    If_No_Please_Explain_In_Detail_What_You_Observed_Translation_count = case_when(
      !is.na(If_No_Please_Explain_In_Detail_What_You_Observed) &
        (If_No_Please_Explain_In_Detail_What_You_Observed_Translation == "translation" |
           is.na(If_No_Please_Explain_In_Detail_What_You_Observed_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_pakage_are_no_or_partially_in_quantities_please_explain_what_you_observe_and_why
    If_pakage_are_no_or_partially_in_quantities_please_explain_what_you_observe_and_why_Translation_count = case_when(
      !is.na(If_pakage_are_no_or_partially_in_quantities_please_explain_what_you_observe_and_why) &
        (If_pakage_are_no_or_partially_in_quantities_please_explain_what_you_observe_and_why_Translation == "translation" |
           is.na(If_pakage_are_no_or_partially_in_quantities_please_explain_what_you_observe_and_why_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_no_or_partially_in_the_same_amonut_of_cash
    If_no_or_partially_in_the_same_amonut_of_cash_translation_count = case_when(
      !is.na(If_no_or_partially_in_the_same_amonut_of_cash) &
        (If_no_or_partially_in_the_same_amonut_of_cash_translation == "translation" |
           is.na(If_no_or_partially_in_the_same_amonut_of_cash_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Please_Explain_In_Detail_What_You_Observed_
    If_Yes_Please_Explain_In_Detail_What_You_Observed_Translation_count = case_when(
      !is.na(If_Yes_Please_Explain_In_Detail_What_You_Observed_) &
        (If_Yes_Please_Explain_In_Detail_What_You_Observed_Translation == "translation" |
           is.na(If_Yes_Please_Explain_In_Detail_What_You_Observed_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_How_Households_Were_Informed_
    Other_Please_Explain_How_Households_Were_Informed_Transilation_count = case_when(
      !is.na(Other_Please_Explain_How_Households_Were_Informed_) &
        (Other_Please_Explain_How_Households_Were_Informed_Transilation == "translation" |
           is.na(Other_Please_Explain_How_Households_Were_Informed_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Specify_Who_Else_Was_On_The_Distribution_Team
    Other_Please_Specify_Who_Else_Was_On_The_Distribution_Team_transilation_count = case_when(
      !is.na(Other_Please_Specify_Who_Else_Was_On_The_Distribution_Team) &
        (Other_Please_Specify_Who_Else_Was_On_The_Distribution_Team_transilation == "translation" |
           is.na(Other_Please_Specify_Who_Else_Was_On_The_Distribution_Team_transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Where_Was_The_Distribution_Team_PresentOther_Please_Specify
    Where_Was_The_Distribution_Team_PresentOther_Please_Specify_Transilation_count = case_when(
      !is.na(Where_Was_The_Distribution_Team_PresentOther_Please_Specify) &
        (Where_Was_The_Distribution_Team_PresentOther_Please_Specify_Transilation == "translation" |
           is.na(Where_Was_The_Distribution_Team_PresentOther_Please_Specify_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail
    Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail_Transiliation_count = case_when(
      !is.na(Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail) &
        (Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail_Transiliation == "translation" |
           is.na(Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail_Transiliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_The_Security_Incidents_In_Detail
    Other_Please_Explain_The_Security_Incidents_In_Detail_Transiliation_count = case_when(
      !is.na(Other_Please_Explain_The_Security_Incidents_In_Detail) &
        (Other_Please_Explain_The_Security_Incidents_In_Detail_Transiliation == "translation" |
           is.na(Other_Please_Explain_The_Security_Incidents_In_Detail_Transiliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Type_Of_Security_Incidents_Were_Observed_During_Distribution
    Type_Of_Security_Incidents_Were_Observed_During_Distribution_Transilation_count = case_when(
      !is.na(Type_Of_Security_Incidents_Were_Observed_During_Distribution) &
        (Type_Of_Security_Incidents_Were_Observed_During_Distribution_Transilation == "translation" |
           is.na(Type_Of_Security_Incidents_Were_Observed_During_Distribution_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Specify_Who_Did_Not_Receive_And_Why
    Please_Specify_Who_Did_Not_Receive_And_Why_Transliation_count = case_when(
      !is.na(Please_Specify_Who_Did_Not_Receive_And_Why) &
        (Please_Specify_Who_Did_Not_Receive_And_Why_Transliation == "translation" |
           is.na(Please_Specify_Who_Did_Not_Receive_And_Why_Transliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Femaleheaded_Households
    Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Femaleheaded_Households_Translation_count = case_when(
      !is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Femaleheaded_Households) &
        (Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Femaleheaded_Households_Translation == "translation" |
           is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Femaleheaded_Households_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Elderly_Headed_Households
    Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Elderly_Headed_Households_Translation_count = case_when(
      !is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Elderly_Headed_Households) &
        (Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Elderly_Headed_Households_Translation == "translation" |
           is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Elderly_Headed_Households_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Households_With_Persons_With_Disabilities
    Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Households_With_Persons_With_Disabilities_Translation_count = case_when(
      !is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Households_With_Persons_With_Disabilities) &
        (Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Households_With_Persons_With_Disabilities_Translation == "translation" |
           is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Households_With_Persons_With_Disabilities_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Idp_Or_Returnee_Households
    Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Idp_Or_Returnee_Households_Translation_count = case_when(
      !is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Idp_Or_Returnee_Households) &
        (Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Idp_Or_Returnee_Households_Translation == "translation" |
           is.na(Please_Specify_Why_Some_Of_The_Households_Didnt_Receive_Assistance_Idp_Or_Returnee_Households_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Please_Explain_The_Problems_In_Detail
    If_Yes_Please_Explain_The_Problems_In_Detail_Translation_count = case_when(
      !is.na(If_Yes_Please_Explain_The_Problems_In_Detail) &
        (If_Yes_Please_Explain_The_Problems_In_Detail_Translation == "translation" |
           is.na(If_Yes_Please_Explain_The_Problems_In_Detail_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Specify_Who_Signed_The_List
    Other_Please_Specify_Who_Signed_The_List_Translation_count = case_when(
      !is.na(Other_Please_Specify_Who_Signed_The_List) &
        (Other_Please_Specify_Who_Signed_The_List_Translation == "translation" |
           is.na(Other_Please_Specify_Who_Signed_The_List_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Specify_How_The_Identity_Of_Beneficiaries_Was_Verified
    Other_Please_Specify_How_The_Identity_Of_Beneficiaries_Was_Verified_Transaltion_count = case_when(
      !is.na(Other_Please_Specify_How_The_Identity_Of_Beneficiaries_Was_Verified) &
        (Other_Please_Specify_How_The_Identity_Of_Beneficiaries_Was_Verified_Transaltion == "translation" |
           is.na(Other_Please_Specify_How_The_Identity_Of_Beneficiaries_Was_Verified_Transaltion)) ~ 1,
      TRUE ~ 0
    ),
    
    # Nothing_Identity_Was_Not_Verified
    Nothing_Identity_Was_Not_Verified_Translation_count = case_when(
      !is.na(Nothing_Identity_Was_Not_Verified) &
        (Nothing_Identity_Was_Not_Verified_Translation == "translation" |
           is.na(Nothing_Identity_Was_Not_Verified_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Reson_selection_of_households_not__follow_the_criteria_outlined_in_the_guidelines
    Reson_selection_of_households_not__follow_the_criteria_outlined_in_the_guidelines_transilation_count = case_when(
      !is.na(Reson_selection_of_households_not__follow_the_criteria_outlined_in_the_guidelines) &
        (Reson_selection_of_households_not__follow_the_criteria_outlined_in_the_guidelines_transilation == "translation" |
           is.na(Reson_selection_of_households_not__follow_the_criteria_outlined_in_the_guidelines_transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Who_Took_The_Charge_Of_Leftover_Packages
    Who_Took_The_Charge_Of_Leftover_Packages_Translation_count = case_when(
      !is.na(Who_Took_The_Charge_Of_Leftover_Packages) &
        (Who_Took_The_Charge_Of_Leftover_Packages_Translation == "translation" |
           is.na(Who_Took_The_Charge_Of_Leftover_Packages_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Explain_Complete_Procurement_Doc_Not_Available
    If_No_Explain_Complete_Procurement_Doc_Not_Available_Transliation_count = case_when(
      !is.na(If_No_Explain_Complete_Procurement_Doc_Not_Available) &
        (If_No_Explain_Complete_Procurement_Doc_Not_Available_Transliation == "translation" |
           is.na(If_No_Explain_Complete_Procurement_Doc_Not_Available_Transliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_The_Issue_In_Detail
    Other_Please_Explain_The_Issue_In_Detail_transliation_count = case_when(
      !is.na(Other_Please_Explain_The_Issue_In_Detail) &
        (Other_Please_Explain_The_Issue_In_Detail_transliation == "translation" |
           is.na(Other_Please_Explain_The_Issue_In_Detail_transliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_Why_The_Cpm_Team_Were_Not_Briefed
    Other_Please_Explain_Why_The_Cpm_Team_Were_Not_Briefed_transalition_count = case_when(
      !is.na(Other_Please_Explain_Why_The_Cpm_Team_Were_Not_Briefed) &
        (Other_Please_Explain_Why_The_Cpm_Team_Were_Not_Briefed_transalition == "translation" |
           is.na(Other_Please_Explain_Why_The_Cpm_Team_Were_Not_Briefed_transalition)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Is_Selected_Why_Were_The_Cpm_Team_Not_Presenttransaltion_count
    If_No_Is_Selected_Why_Were_The_Cpm_Team_Not_Presenttransaltion_count = case_when(
      !is.na(If_No_Is_Selected_Why_Were_The_Cpm_Team_Not_Present_) &
        (If_No_Is_Selected_Why_Were_The_Cpm_Team_Not_Presenttransaltion == "translation" |
           is.na(If_No_Is_Selected_Why_Were_The_Cpm_Team_Not_Presenttransaltion)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Please_Explain_How_The_Wba_Was_Used
    If_Yes_Please_Explain_How_The_Wba_Was_Used_Translation_count = case_when(
      !is.na(If_Yes_Please_Explain_How_The_Wba_Was_Used) &
        (If_Yes_Please_Explain_How_The_Wba_Was_Used_Translation == "translation" |
           is.na(If_Yes_Please_Explain_How_The_Wba_Was_Used_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Please_Explain_Why_Targeting_Was_Not_Done_Based_On_The_Wba
    If_No_Please_Explain_Why_Targeting_Was_Not_Done_Based_On_The_Wba_Tranlsation_count = case_when(
      !is.na(If_No_Please_Explain_Why_Targeting_Was_Not_Done_Based_On_The_Wba) &
        (If_No_Please_Explain_Why_Targeting_Was_Not_Done_Based_On_The_Wba_Tranlsation == "translation" |
           is.na(If_No_Please_Explain_Why_Targeting_Was_Not_Done_Based_On_The_Wba_Tranlsation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Why_Was_This
    If_No_Why_Was_This_Translation_count = case_when(
      !is.na(If_No_Why_Was_This) &
        (If_No_Why_Was_This_Translation == "translation" |
           is.na(If_No_Why_Was_This_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_How_Did_You_Select_The_Beneficiaries
    If_Yes_How_Did_You_Select_The_Beneficiaries_Transliation_count = case_when(
      !is.na(If_Yes_How_Did_You_Select_The_Beneficiaries) &
        (If_Yes_How_Did_You_Select_The_Beneficiaries_Transliation == "translation" |
           is.na(If_Yes_How_Did_You_Select_The_Beneficiaries_Transliation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Please_Explain_The_Problems_In_Detail_
    If_Yes_Please_Explain_The_Problems_In_Detail__Translation_count = case_when(
      !is.na(If_Yes_Please_Explain_The_Problems_In_Detail_) &
        (If_Yes_Please_Explain_The_Problems_In_Detail__Translation == "translation" |
           is.na(If_Yes_Please_Explain_The_Problems_In_Detail__Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Some_Of_The_List_Matched_Please_Explain
    Some_Of_The_List_Matched_Please_Explain_Transilation_count = case_when(
      !is.na(Some_Of_The_List_Matched_Please_Explain) &
        (Some_Of_The_List_Matched_Please_Explain_Transilation == "translation" |
           is.na(Some_Of_The_List_Matched_Please_Explain_Transilation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Explain_Any_Conflicts_Arguments_Grievances_Raised_In_Detail
    If_No_Explain_Why_Not_The_Household_List_Did_Not_Match_The_Sig_Disbursement_Request_Form_Translation_count = case_when(
      !is.na(If_No_Explain_Why_Not_The_Household_List_Did_Not_Match_The_Sig_Disbursement_Request_Form) &
        (If_No_Explain_Why_Not_The_Household_List_Did_Not_Match_The_Sig_Disbursement_Request_Form_Translation == "translation" |
           is.na(If_No_Explain_Why_Not_The_Household_List_Did_Not_Match_The_Sig_Disbursement_Request_Form_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_No_Please_Explain_Why_The_Household_List_Was_Not_Available
    If_No_Please_Explain_Why_The_Household_List_Was_Not_Available_Translation_count = case_when(
      !is.na(If_No_Please_Explain_Why_The_Household_List_Was_Not_Available) &
        (If_No_Please_Explain_Why_The_Household_List_Was_Not_Available_Translation == "translation" |
           is.na(If_No_Please_Explain_Why_The_Household_List_Was_Not_Available_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Remarks
    Remarks_translation_count = case_when(
      !is.na(Remarks) &
        (Remarks_translation == "translation" |
           is.na(Remarks_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Facilitating_Partner_other
    Facilitating_Partner_other_count = case_when(
      Encoding(Facilitating_Partner_other) %in% "UTF-8" ~ 1,
      TRUE ~ 0
    ),
    
    # Interviewee_Respondent_Type
    # Interviewee_Respondent_Type_count = case_when(
    #   Encoding(Interviewee_Respondent_Type) %in% "UTF-8" ~ 1,
    #   TRUE ~ 0
    # )
  )
  
  
  do_missing_translation <- direct_observation %>%  select(SubmissionDate_only_date, Starttime_only_date, KEY,Province, District, ends_with("_count"), review_status, Status_QA_Log = Status, weekly_reporting_round) 
  do_missing_translation <- do_missing_translation %>% select(-c(rep_If_Goods_What_Is_The_Standard_Quantity_Of_Relief_Items_Distributed_In_Each_Package_According_To_The_Distribution_Team_count,
                                                                 rep_Item_And_Quantity_Of_Random_Package_1_count,
                                                                 rep_Item_And_Quantity_Of_Random_Package_2_count,
                                                                 Rep_Item_and_quantity_of_random_package_3_count
  ))
  
  
  do_missing_translation <- do_missing_translation %>% 
    rowwise() %>% 
    mutate(Total_Missing_translation = sum(c_across(ends_with("_count"))),
           weekly_reporting_round = case_when(
             is.na(weekly_reporting_round) ~ 999,
             TRUE ~ weekly_reporting_round
           ),
           translation_status = case_when(
             Total_Missing_translation == 0 ~ "Complete", 
             Total_Missing_translation > 0 ~ "Incomplete", 
           )) %>% arrange(desc(Total_Missing_translation))
  
  
  
  return(do_missing_translation)
  
}

# Form1 -------------------------------------------------------------------
form1_translation_check = function(Form_1){
  
  
  Form_1 <- Form_1 %>% mutate(
    # Are_There_Any_People_Not_Originally_From_This_Village_That_Have_Moved_Here_If_Yes_Who
    Are_There_Any_People_Not_Originally_From_This_Village_That_Have_Moved_Here_If_Yes_Who_count = case_when(
      !is.na(Are_There_Any_People_Not_Originally_From_This_Village_That_Have_Moved_Here_Yes_Who) &
        is.na(Are_There_Any_People_Not_Originally_From_This_Village_That_Have_Moved_Here_If_Yes_Who) ~ 1,
      TRUE ~ 0
    ),
    
    # Other_Please_Specify
    Other_Please_Specify_Translation_count = case_when(
      !is.na(Other_Please_Specify) &
        (Other_Please_Specify_Translation == "translation" |
           is.na(Other_Please_Specify_Translation)) ~ 1,
      TRUE ~ 0
    ),
    # How_Did_The_Household_Listing_Happen_In_Your_Community
    How_Did_The_Household_Listing_Happen_In_Your_Community__Translation_count = case_when(
      !is.na(How_Did_The_Household_Listing_Happen_In_Your_Community) &
        (How_Did_The_Household_Listing_Happen_In_Your_Community__Translation == "translation" |
           is.na(How_Did_The_Household_Listing_Happen_In_Your_Community__Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # If_Yes_Can_You_Explain_The_Issueschallenges_Experienced_During_The_Household_Listing_Process
    If_Yes_Can_You_Explain_The_Issueschallenges_Experienced_During_The_Household_Listing_Process_Translation_count = case_when(
      !is.na(If_Yes_Can_You_Explain_The_Issueschallenges_Experienced_During_The_Household_Listing_Process) &
        (If_Yes_Can_You_Explain_The_Issueschallenges_Experienced_During_The_Household_Listing_Process_Translation == "translation" |
           is.na(If_Yes_Can_You_Explain_The_Issueschallenges_Experienced_During_The_Household_Listing_Process_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Who_Did_They_Complain_To_Other
    Who_Did_They_Complain_To_Other_Translaiton_count = case_when(
      !is.na(Who_Did_They_Complain_To_Other) &
        (Who_Did_They_Complain_To_Other_Translaiton == "translation" |
           is.na(Who_Did_They_Complain_To_Other_Translaiton)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # What_Was_The_Nature_Of_The_Complaints_Other
    What_Was_The_Nature_Of_The_Complaints_Other_Transaltion_count = case_when(
      !is.na(What_Was_The_Nature_Of_The_Complaints_Other) &
        (What_Was_The_Nature_Of_The_Complaints_Other_Transaltion == "translation" |
           is.na(What_Was_The_Nature_Of_The_Complaints_Other_Transaltion)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # If_Yes_Please_Specify_How_It_Was_Resolved
    If_Yes_Please_Specify_How_It_Was_Resolved_Translation_count = case_when(
      !is.na(If_Yes_Please_Specify_How_It_Was_Resolved) &
        (If_Yes_Please_Specify_How_It_Was_Resolved_Translation == "translation" |
           is.na(If_Yes_Please_Specify_How_It_Was_Resolved_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Households_Belong_To_Any_Of_The_Following_Groups_Other_Please_Specify
    Households_Belong_To_Any_Of_The_Following_Groups_Other_Please_Specify_translation_count = case_when(
      !is.na(Households_Belong_To_Any_Of_The_Following_Groups_Other_Please_Specify) &
        (Households_Belong_To_Any_Of_The_Following_Groups_Other_Please_Specify_translation == "translation" |
           is.na(Households_Belong_To_Any_Of_The_Following_Groups_Other_Please_Specify_translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Why_Were_These_Households_Not_Included
    Why_Were_These_Households_Not_Included_Translation_count = case_when(
      !is.na(Why_Were_These_Households_Not_Included) &
        (Why_Were_These_Households_Not_Included_Translation == "translation" |
           is.na(Why_Were_These_Households_Not_Included_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # If_Not_Why_Have_They_Not_Been_Considered
    If_Not_Why_Have_They_Not_Been_Considered__Translation_count = case_when(
      !is.na(If_Not_Why_Have_They_Not_Been_Considered) &
        (If_Not_Why_Have_They_Not_Been_Considered__Translation == "translation" |
           is.na(If_Not_Why_Have_They_Not_Been_Considered__Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Can_You_Describe_Whowhich_Community_Groups_You_Notice_Are_Being_Included_That_You_Think_Should_Not_Be
    Can_You_Describe_Whowhich_Community_Groups_You_Notice_Are_Being_Included_That_You_Think_Should_Not_Be_Translation_count = case_when(
      !is.na(Can_You_Describe_Whowhich_Community_Groups_You_Notice_Are_Being_Included_That_You_Think_Should_Not_Be) &
        (Can_You_Describe_Whowhich_Community_Groups_You_Notice_Are_Being_Included_That_You_Think_Should_Not_Be_Translation == "translation" |
           is.na(Can_You_Describe_Whowhich_Community_Groups_You_Notice_Are_Being_Included_That_You_Think_Should_Not_Be_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Can_You_Explain_Why_You_Think_The_Households_Mentioned_Should_Not_Be_Receiving_Covid19_Relief
    Can_You_Explain_Why_You_Think_The_Households_Mentioned_Should_Not_Be_Receiving_Covid19_Relief_Translation_count = case_when(
      !is.na(Can_You_Explain_Why_You_Think_The_Households_Mentioned_Should_Not_Be_Receiving_Covid19_Relief) &
        (Can_You_Explain_Why_You_Think_The_Households_Mentioned_Should_Not_Be_Receiving_Covid19_Relief_Translation == "translation" |
           is.na(Can_You_Explain_Why_You_Think_The_Households_Mentioned_Should_Not_Be_Receiving_Covid19_Relief_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Are_You_Aware_Of_Any_Other_Issues_Or_Concerns_In_Planning_The_Distribution_Process_For_Covid19_Relief_Response_audio
    Are_You_Aware_Of_Any_Other_Issues_Or_Concerns_In_Planning_The_Distribution_Process_For_Covid19_Relief_Response_audio_Translation_count = case_when(
      !is.na(Are_You_Aware_Of_Any_Other_Issues_Or_Concerns_In_Planning_The_Distribution_Process_For_Covid19_Relief_Response_audio) &
        (Are_You_Aware_Of_Any_Other_Issues_Or_Concerns_In_Planning_The_Distribution_Process_For_Covid19_Relief_Response_audio_Translation == "translation" |
           is.na(Are_You_Aware_Of_Any_Other_Issues_Or_Concerns_In_Planning_The_Distribution_Process_For_Covid19_Relief_Response_audio_Translation)) ~ 1,
      TRUE ~ 0
    )
    ,
    
    # Remarks
    Remarks_translation_count = case_when(
      !is.na(Remarks) &
        (Remarks_translation == "translation" |
           is.na(Remarks_translation)) ~ 1,
      TRUE ~ 0
    ),
    # Facilitating_Partner_other
    Facilitating_Partner_other_count = case_when(
      Encoding(Facilitating_Partner_other) %in% "UTF-8" ~ 1,
      TRUE ~ 0
    ),
    # Interviewee_Respondent_Type
    Interviewee_Respondent_Type_count = case_when(
      Encoding(Interviewee_Respondent_Type) %in% "UTF-8" ~ 1,
      TRUE ~ 0
    ),
    
  )

  form1_missing_translation <- Form_1 %>%  select(SubmissionDate_only_date, Starttime_only_date, KEY,Province, District, ends_with("_count"), Status_QA_Log = Status, weekly_reporting_round) 
  
  form1_missing_translation <- form1_missing_translation %>% 
    rowwise() %>% 
    mutate(Total_Missing_translation = sum(c_across(ends_with("_count"))),
           weekly_reporting_round = case_when(
             is.na(weekly_reporting_round) ~ 999,
             TRUE ~ weekly_reporting_round
           ),
           translation_status = case_when(
             Total_Missing_translation == 0 ~ "Complete", 
             Total_Missing_translation > 0 ~ "Incomplete", 
           )) %>% arrange(desc(Total_Missing_translation))
  
  return(form1_missing_translation)

}

# form2 -------------------------------------------------------------------
form2_translation_check = function(Form_2){
  
  Form_2 <- Form_2 %>% mutate(
    # How_Did_You_Find_Out_About_The_Covid19_Response_Other
    How_Did_You_Find_Out_About_The_Covid19_Response_Other_translation_count = case_when(
      !is.na(How_Did_You_Find_Out_About_The_Covid19_Response_Other) &
        (How_Did_You_Find_Out_About_The_Covid19_Response_Other_translation == "translation" |
           is.na(How_Did_You_Find_Out_About_The_Covid19_Response_Other_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Who_Did_You_Have_To_Pay_Other
    If_Yes_Who_Did_You_Have_To_Pay_Other_translation_count = case_when(
      !is.na(If_Yes_Who_Did_You_Have_To_Pay_Other) &
        (If_Yes_Who_Did_You_Have_To_Pay_Other_translation == "translation" |
           is.na(If_Yes_Who_Did_You_Have_To_Pay_Other_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Explain_How_Households_Are_Being_Selected
    Explain_How_Households_Are_Being_Selected_Translation_count = case_when(
      !is.na(Explain_How_Households_Are_Being_Selected) &
        (Explain_How_Households_Are_Being_Selected_Translation == "translation" |
           is.na(Explain_How_Households_Are_Being_Selected_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Does_Your_Household_Have_Any_Of_The_Following_Vulnerabilities_Other
    Does_Your_Household_Have_Any_Of_The_Following_Vulnerabilities_Other_translation_count = case_when(
      !is.na(Does_Your_Household_Have_Any_Of_The_Following_Vulnerabilities_Other) &
        (Does_Your_Household_Have_Any_Of_The_Following_Vulnerabilities_Other_translation == "translation" |
           is.na(Does_Your_Household_Have_Any_Of_The_Following_Vulnerabilities_Other_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Do_You_Have_Any_Further_Comments_Or_Concerns_About_The_Household_Listing_Process__conserns
    Do_You_Have_Any_Further_Comments_Or_Concerns_About_The_Household_Listing_Process__conserns_translation_count = case_when(
      !is.na(Do_You_Have_Any_Further_Comments_Or_Concerns_About_The_Household_Listing_Process__conserns) &
        (Do_You_Have_Any_Further_Comments_Or_Concerns_About_The_Household_Listing_Process__conserns_translation == "translation" |
           is.na(Do_You_Have_Any_Further_Comments_Or_Concerns_About_The_Household_Listing_Process__conserns_translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # If_Yes_Please_Specify_The_Type_Of_Concern_You_Have_About_Accessing_Assistance_other
    If_Yes_Please_Specify_The_Type_Of_Concern_You_Have_About_Accessing_Assistance_other_Translation_count = case_when(
      !is.na(If_Yes_Please_Specify_The_Type_Of_Concern_You_Have_About_Accessing_Assistance_other) &
        (If_Yes_Please_Specify_The_Type_Of_Concern_You_Have_About_Accessing_Assistance_other_Translation == "translation" |
           is.na(If_Yes_Please_Specify_The_Type_Of_Concern_You_Have_About_Accessing_Assistance_other_Translation)) ~ 1,
      TRUE ~ 0
    ),
    
    # Households_Or_Families_That_Are_Isolated_Due_To_Community_Conflict_Please_Other
    Households_Or_Families_That_Are_Isolated_Due_To_Community_Conflict_Please_Other_count = case_when(
      household_that_the_respondent_belongs_to %in% "77" &
        Encoding(Households_Or_Families_That_Are_Isolated_Due_To_Community_Conflict_Please_Other) %in% "UTF-8" ~ 1,
      TRUE ~ 0
    ),
    
    # Remarks
    Remarks_translation_count = case_when(
      !is.na(Remarks) &
        (Remarks_translation == "translation" |
           is.na(Remarks_translation)) ~ 1,
      TRUE ~ 0
    )
  )
  
  form2_missing_translation <- Form_2 %>%  select(SubmissionDate_only_date, Starttime_only_date,KEY,Province, District, ends_with("_count"), Status_QA_Log = Status, weekly_reporting_round) 
  
  
  form2_missing_translation <- form2_missing_translation %>% 
    rowwise() %>% 
    mutate(Total_Missing_translation = sum(c_across(ends_with("_count"))),
           weekly_reporting_round = case_when(
             is.na(weekly_reporting_round) ~ 999,
             TRUE ~ weekly_reporting_round
           ),
           translation_status = case_when(
             Total_Missing_translation == 0 ~ "Complete", 
             Total_Missing_translation > 0 ~ "Incomplete", 
           )) %>% arrange(desc(Total_Missing_translation))
  
  return(form2_missing_translation)
  
}
