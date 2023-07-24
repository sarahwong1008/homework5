#' Get Data Point of Interest for a Player in a Specific Year
#'
#' This function returns a data point of interest for a specific player in a given year.
#' @param year The year of interest.
#' @param player The player's name.
#' @return The data point of interest for the specified player and year.
#' @export
#' @examples
#' get_player_data_point(2019, "LeBron James")

get_player_data_point <- function(year, player) {
  # Filter the dataset to get the data point for the specified player in the given year
  player_data_point <- subset(player_data, Player == player & Year == year)

  if (nrow(player_data_point) == 0) {
    # Player data not found for the given year
    return(NULL)
  } else {
    # Return the data point for the player in the given year
    return(player_data_point)
  }
}
