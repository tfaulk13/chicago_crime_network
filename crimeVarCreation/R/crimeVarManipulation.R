
#' Function to use a list to alter crime_type column
#' @param list 
#' @param dataframe 
#' @param columnToUse 
#' @param columnToChange 
#' @param varInput
#' @export
gen_crime_type <- function(list, data = df, columnToUse, columnToChange, varInput){
  
  # Setting up column names to use
  col1 <- deparse(substitute(columnToUse))
  col2 <- deparse(substitute(columnToChange))
  
  # Changing the row entry based on the list of strings
  for(x in list){
    data[[col2]] <- ifelse(str_detect(as.character(data[[col1]]), x), varInput, as.character(data[[col2]]))
  }
  
  return(data)
}

#' Function to create dummy variable based on a list of strings
#' @param list 
#' @param dataframe 
#' @param columnToUse 
#' @param columnToCreate
#' @export
Crime_type_dummies <- function(list, data = df, columnToUse, columnToCreate){
  
  # Setting up column names to use
  col1 <- deparse(substitute(columnToUse))
  col2 <- deparse(substitute(columnToCreate))
  
  # Initialize new column
  data[, col2] <- 0 
  
  # Changing the row entry based on the list of strings
  for(x in list){
    data[[col2]] <- ifelse(str_detect(as.character(data[[col1]]), x), 1, data[[col2]])
  }
  
  return(data)
}

#' This function takes in a dataframe and a columntouse from the dataframe
#' and generates two new columns. Your data should be in long format
#' as the first new column generated will be a dummy variable column for
#' columnToUse. The second column is a sum of the columnToUse. Each of 
#' these values is passed to all other rows with the same UID.
#' Need to generalize to beyond just UID.
#' @param dataframe 
#' @param columnToUse 
#' @param NewColumn1 
#' @param NewColumn2
#' @export
AggregatedColumns <- function(DF, columnToUse, NewCol1, NewCol2) {
  
  # Setting up column names to use
  columnToUse <- deparse(substitute(columnToUse))
  NewCol1 <- deparse(substitute(NewCol1))
  NewCol2 <- deparse(substitute(NewCol2))
  
  #### Creating new columns 
  # Creating the new column (one simple line)
  DF[[NewCol1]] <- as.integer(DF$UID %in% DF$UID[DF[[columnToUse]] == 1])
  
  # Counting up total offenses
  mutate_call = lazyeval::interp(~sum(a), a = as.name(columnToUse))
  DF <- DF %>% group_by_("UID") %>% mutate_(.dots = setNames(list(mutate_call), NewCol2))
  
  DF
}