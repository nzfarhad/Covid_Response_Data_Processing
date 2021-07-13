
type_cast <- function(x){
  if(!is.numeric(x)){
    x <- gsub(",", ".", x)
    y <- if_else(x %in% c("Data_Clerk", "data_clerk", "Data_Clerk1", "Data_Clerk10", "[blank]", "blank"), NA_character_, x )
    y <- as.numeric(y)
    return(y)
  } else {
    return(x)
  }
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# sum return NA for empty set instead of zero
custom_sum <- function(x){
  if(all(is.na(x))){
    NA_real_
  } else{
    sum(x, na.rm = T)
  }
}


fix_attachments <- function(df, server) {
  col_index <- grepl("File skipped from exports", df)
  col_names <- df %>% 
    select(which(col_index)) %>% 
    colnames()
  server_name <- paste0(server, ".surveycto.com/view/submission-attachment/")
  
  df <- df %>% 
    mutate(across(col_names,
                  function(x)
                    x = case_when(
                      !is.na(x) ~ paste0(server_name,
                                         str_remove(x, "File skipped from exports: "),
                                         "?uuid=uuid%3A",
                                         str_remove(str_remove(substr(KEY, 1, 41), "uuid:"), "uuid:")
                      ),
                      TRUE ~ x
                    )
    ))
  
  return(df)
}

fix_attachments_col_specific <- function(df, server, vars) {
 
  col_names <- vars
  server_name <- paste0(server, ".surveycto.com/view/submission-attachment/")
  
  df <- df %>% 
    mutate(across(col_names,
                  function(x)
                    x = case_when(
                      !is.na(x) ~ paste0(server_name,
                                         str_remove(x, "File skipped from exports: "),
                                         "?uuid=uuid%3A",
                                         str_remove(str_remove(substr(KEY, 1, 41), "uuid:"), "uuid:")
                      ),
                      TRUE ~ x
                    )
    ))
  
  return(df)
}
