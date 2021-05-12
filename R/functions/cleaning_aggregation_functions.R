
type_cast <- function(x){
  if(!is.numeric(x)){
    y <- if_else(x %in% c("Data_Clerk", "data_clerk", "Data_Clerk1", "Data_Clerk10", "[blank]", "blank"), NA_character_, x )
    y <- as.numeric(x)
    return(y)
  } else {
    return(x)
  }
}