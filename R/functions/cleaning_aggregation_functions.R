
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

