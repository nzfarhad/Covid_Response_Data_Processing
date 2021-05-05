# label SurveyCTO Datasets

labeler <- function(data, tool, survey_label = "label:English", choice_lable = "label:English"){

  survey_questions <- read_excel(tool, "survey")
  survey_choices <- read_excel(tool,"choices")
  
  if("value" %in% names(survey_choices)){
    names(survey_choices)[names(survey_choices) == "value"] <- "name"
  }
  
  # Prep Survey Questions
  survey_questions <- survey_questions[grepl("\\bselect_", survey_questions$type),]
  survey_questions$select_type <- survey_questions$type %>% str_replace_all(" .*","")
  survey_questions$type <- survey_questions$type %>% str_replace_all("select_one ", "") %>% str_replace_all("select_multiple ", "")
  survey_questions <- survey_questions %>%
    dplyr::select(type, name, select_type, all_of(survey_label))
  
  # Prep Choices
  survey_choices$name <- survey_choices$name %>% as.character
  survey_choices <- survey_choices[!is.na(survey_choices$list_name),]
  
  for(var in names(data)){
    if(var %in% survey_questions$name){
      # Grab choices for that variable
      survey_choices_i <- survey_choices[survey_choices$list_name %in% survey_questions$type[survey_questions$name %in% var],]
      
      # select one
      if(survey_questions$select_type[survey_questions$name %in% var] == "select_one"){
        
        for(choice_i in 1:nrow(survey_choices_i)){
          data[[var]] <- data[[var]] %>% 
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"), survey_choices_i[[choice_lable]][choice_i])
        }
        
        # select multiple  
      } else{
        for(choice_i in 1:nrow(survey_choices_i)){
          data[[var]] <- data[[var]] %>% 
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"), survey_choices_i[[choice_lable]][choice_i])
        }
      }
      
    }
  }
  
  return(data)
  
}
