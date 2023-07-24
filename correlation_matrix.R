#' Get Correlation Matrix for Numeric Variables in the NBA Data for a Specific Year
#'
#' This function returns the correlation matrix for numeric variables in the NBA data for a given year.
#' @param year The year of interest.
#' @return The correlation matrix for numeric variables in the NBA data for the specified year.
#' @export
#' @examples
#' get_correlation_matrix(2019)

get_correlation_matrix <- function(year) {
  # Filter the dataset to get data for the specified year
  data_in_year <- subset(data, Year == year)

  if (nrow(data_in_year) == 0) {
    # No data found for the given year
    return(NULL)
  } else {
    # Select numeric continuous variables (excluding categorical data if any)
    numeric_vars <- sapply(data_in_year, is.numeric)
    numeric_data <- data_in_year[, numeric_vars]

    # Calculate the correlation matrix for numeric variables
    correlation_matrix <- cor(numeric_data)

    # Return the correlation matrix
    return(correlation_matrix)
  }
}
