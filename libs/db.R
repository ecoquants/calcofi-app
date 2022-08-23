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

# test connection:
# dbListTables(con)


# helper functions ----
glue2 <- function(x, null_str="", .envir = sys.frame(-3), ...){
  # handle NULLs inside glue string as empty character
  null_transformer <- function(str = "NULL") {
    function(text, envir) {
      out <- glue::identity_transformer(text, envir)
      if (is.null(out))
        return(str)
      out }}
  glue(x, .transformer = null_transformer(null_str), .envir = .envir, ...)
}
