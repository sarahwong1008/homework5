#' Get the Oldest Player in the NBA Data for a Specific Year
#'
#' This function returns the oldest player in the NBA data for a given year.
#' @param year The year of interest.
#' @return The name of the oldest player in the NBA data for the specified year.
#' @export
#' @examples
#' get_oldest_player(2019)

get_oldest_player <- function(year) {
  # Filter the dataset to get the players' data for the specified year
  players_in_year <- subset(nba_data, Year == year)

  if (nrow(players_in_year) == 0) {
    # No player data found for the given year
    return(NULL)
  } else {
    # Find the oldest player in the specified year
    oldest_age <- max(players_in_year$Age)
    oldest_players <- subset(players_in_year, Age == oldest_age)

    # If multiple players have the same age, return the first one (you can sort by any other criteria)
    oldest_player <- oldest_players$Player[1]
    return(oldest_player)
  }
}
