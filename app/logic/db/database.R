## Function for queries to mybb
box::use(
  DBI,
  glue,
  RMySQL,
)

dbHost <- Sys.getenv("HOST")
dbPort <- Sys.getenv("PORT") |> as.integer()
dbUser <- Sys.getenv("DBUSER")
dbPassword <- Sys.getenv("DBPASSWORD")

sqlQuery <- function(query, db) {
  con <-
    DBI$dbConnect(
      RMySQL$MySQL(),
      dbname = Sys.getenv(db),
      host = dbHost,
      port = dbPort,
      user = dbUser,
      password = dbPassword
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
