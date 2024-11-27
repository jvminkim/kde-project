list_of_packages = c("DBI", "RPostgres")
lapply(list_of_packages, library, character.only=TRUE)

#' Establish PostgreSQL connection
#' 
#' @return 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' }
#' 

get_connection = function() {
  con = dbConnect(
    RPostgres::Postgres(),
    dbname = "statcast",
    host = "localhost",
    port = 5432,
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
  return(con)
}

con = get_connection()

#' Get pitch-by-pitch data
#' 
#' @param con SQL server connection
#' @param years Years to be pulled
#' @param batted_ball Batted balls only or all pitches: 1- only batted balls, 0- all pitches
#' 
#' @return Dataframe of pitch-by-pitch data
#' @export
#' 
#' @examples
#' \dontrun{
#' bip_data = retrieve_pbp_data(con,2017:2023, 1)
#' pbp_data = retrieve_pbp_data(con,2017:2023, 0)
#' }

get_pbp_data = function(con, years, batted_ball) {
  query_year = function(year) {
    if (batted_ball == 1) {
      query = paste0(
        "SELECT * FROM statcast_all 
         WHERE hc_x IS NOT NULL AND
               hc_y IS NOT NULL AND
               launch_angle IS NOT NULL AND
               launch_speed IS NOT NULL AND
               game_year = ", year, ";"
      )
    } else {
      query = paste0(
        "SELECT * FROM statcast_all 
         WHERE game_year = ", year, ";"
      )
    }
    dbGetQuery(con, query)
  }
  
  pbp_data_list = lapply(years, query_year)
  
  pbp_data = do.call(rbind, pbp_data_list)
  return(pbp_data)
}

bip_data = get_pbp_data(con,2017:2023, 1)

#' Get player heights
#'
#' @param con Connection to postgreSQL server
#'
#' @return Dataframe of player heights
#' @export
#' 
#' @examples
#' \dontrun {
#' player_heights = get_player_height(con)
#' }
#' 
get_player_height = function(con) {
  query = paste0(
    "SELECT * FROM player_heights;"
  )
  player_heights = dbGetQuery(con, query)
  return(player_heights)
}
player_heights = get_player_height(con)

#' Transform hc_x and hc_y coordinates
#' 
#' @param data Pbp data
#' 
#' @return Dataframe with fixed hit coordinates
#' @export
#' 
#' @examples
#' \dontrun{
#' bip_data = mlbam_xy_transformation(bip_data)
#' }

mlbam_xy_transformation = function(data) {
  dplyr::mutate(data, x_land = (hc_x - 125.42) * 2.5,
                      y_land = (198.27 - hc_y) * 2.5)
}

bip_data = mlbam_xy_transformation(bip_data)

