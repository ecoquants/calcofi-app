# packages ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}
librarian::shelf(
  DBI, dbplyr, dplyr, here, RPostgres,
  quiet = T)

is_server <- Sys.info()[["sysname"]] == "Linux"
host <- ifelse(
  is_server,
  "postgis",   # from rstudio to postgis docker container on server
  "localhost") # from laptop to locally tunneled connection to db container on server
# for localhost db, see: https://github.com/calcofi/server#ssh-tunnel-connection-to-postgis-db
db_pass_txt <- ifelse(
  is_server,
  "/share/.calcofi_db_pass.txt",
  "~/.calcofi_db_pass.txt")
stopifnot(file.exists(db_pass_txt))

# database connect ----
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  host     = host, 
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt))

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
q <- function(sql){ dbSendQuery(con, sql) }

# test connection:
# dbListTables(con)
