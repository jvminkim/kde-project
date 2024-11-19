list_of_packages = c("DBI", "RPostgres")
lapply(list_of_packages, library, character.only=TRUE)

con = dbConnect(
  RPostgres::Postgres(),
  dbname = "statcast",
  host = "localhost",
  port = 5432,
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

bip_data = dbGetQuery(con, "SELECT * FROM statcast_all
                                 WHERE hc_x IS NOT NULL AND
                                       hc_y IS NOT NULL;")
bip_data = dplyr::mutate(bip_data, x_land = hc_x - 125.42, y_land = 198.27 - hc_y)

mlbam_xy_transformation = function(data) {
  dplyr::mutate(data, x_land = (hc_x - 125.42) * 2.5,
                      y_land = (198.27 - hc_y) * 2.5)
}
bip_data = mlbam_xy_transformation(bip_data)

bip_na = dplyr::filter(bip_na, )

bip2023 = dplyr::filter(pitches2023, !is.na(hc_x) & !is.na(hc_y))
bip_data_sub_2023 = dplyr::filter(bip_data, game_year == 2023)
