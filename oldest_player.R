#' Get the Oldest Player in the NBA Data for a Specific Year
#'
#' This function returns the oldest player in the NBA data for a given year.
#' @param year The year of interest.
#' @return The name of the oldest player in the NBA data for the specified year.
#' @export
#' @examples
#' get_oldest_player(2019)

get_oldest_player <- function(year) {
  # Subset the data for the given year
  data_year <- data[data$Year == year,]
 
  # Find the oldest player(s)
  oldest_players <- data_year[data_year$Age == max(data_year$Age),]
 
  # If multiple players have the same age, return the one with the most points
  oldest_player <- oldest_players[which.max(oldest_players$PTS),]$Player
 
  # Return the player's name
  return(paste("The oldest player in", year, "was", oldest_player, "aged", max(data_year$Age)))
}
