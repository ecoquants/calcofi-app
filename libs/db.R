# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, here, RPostgres)

# database connect ----
db_pass_txt <- "~/.calcofi_db_pass.txt"
stopifnot(file.exists(db_pass_txt))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  # host     = "db.calcofi.io", # from laptop to server
  # host     = "localhost",     # from laptop to local db
  host     = "postgis",         # from server to docker containers
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt))

q <- function(sql){ dbSendQuery(con, sql) }

# test connection:
# dbListTables(con)
