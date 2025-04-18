## Function for queries to mybb
box::use(
  config,
  DBI,
  glue,
  RMySQL,
)

db_host <- Sys.getenv("HOST")
db_port <- Sys.getenv("PORT") |> as.integer()
db_user <- Sys.getenv("DBUSER")
db_password <- Sys.getenv("DBPASSWORD")

sqlQuery <- function(query, db) {
  con <-
    DBI$dbConnect(
      RMySQL$MySQL(),
      dbname = Sys.getenv(db),
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_password
    )

  DBI$dbSendQuery(con, "SET NAMES utf8mb4;")
  DBI$dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbSendQuery(con, "SET character_set_connection=utf8mb4;")

  req <- glue$glue_sql(query, .con = con)

  req <- DBI$dbSendQuery(con, req)
  res <- DBI$dbFetch(req, n = -1)

  DBI$dbClearResult(req)

  DBI$dbDisconnect(con)

  res
}

#' Function for queries to mybb
#' @export
mybbQuery <- function(query) {
  sqlQuery(query, "mybb")
}

#' Function for queries to portal
#' @export
portalQuery <- function(query) {
  sqlQuery(query, "portal")
}

#' Function for queries to index
#' @export
indexQuery <- function(query) {
  sqlQuery(query, "index")
}

#' Function for queries to budget
#' @export
budgetQuery <- function(query) {
  sqlQuery(query, "budget")
}
