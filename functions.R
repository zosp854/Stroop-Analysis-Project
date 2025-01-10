create_summary <- function(data, subject_start, subject_end) {
  
  if (nrow(data) < 10) {
    stop("data is too short")  # Print error message and stop execution
  }
  
data <- subset(data, subject >= subject_start & subject <= subject_end)
    
  results <- data.frame(
    variable = character(),  # Variable name
    statistic = character(),  # Type of statistic
    value = character(),  # Statistical value
    stringsAsFactors = FALSE
  )
  
  # Loop through all variables in the Data Frame
  for (var_name in names(data)) {
    # Extract the variable
    variable <- data[[var_name]]
    
    # Check if the variable is numeric/continuous
    if (class(variable) %in% c("numeric", "integer")) {
      # Calculate continuous statistics
      min_val <- min(variable, na.rm = TRUE)
      max_val <- max(variable, na.rm = TRUE)
      mean_val <- mean(variable, na.rm = TRUE)
      
      # Append results
      results <- rbind(
        results,
        data.frame(variable = var_name, statistic = "Minimum", value = as.character(min_val)),
        data.frame(variable = var_name, statistic = "Maximum", value = as.character(max_val)),
        data.frame(variable = var_name, statistic = "Mean", value = as.character(mean_val))
      )
      
    } else if (class(variable) %in% c("factor", "character")) {
      # Calculate statistics for categorical variables
      levels_summary <- table(variable)
      
      # Append results for each level in the categorical variable
      for (level in names(levels_summary)) {
        results <- rbind(
          results,
          data.frame(
            variable = var_name,
            statistic = paste("Level:", level),
            value = as.character(levels_summary[[level]])
          )
        )
      }
    }
  }
  
  return(results)
}