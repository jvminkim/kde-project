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

statcast_data = dbGetQuery(con, "SELECT * FROM statcast_all;")
