## Function for queries to mybb
box::use(
  DBI,
  glue,
  RMySQL,
)

dbHost <- "127.0.0.1"
dbPort <- 3306
dbUser <- "root"
dbPassword <- "password"

sqlQuery <- function(query, db) {
  tryCatch({
    con <-
      DBI$dbConnect(
        RMySQL$MySQL(),
        dbname = db,
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
    
    res
  },
  finally = {
    DBI$dbDisconnect(con)
  })
}

#' Function for queries to mybb
#' @export
mybbQuery <- function(query) {
  sqlQuery(query, "mybbdb")
}

#' Function for queries to portal
#' @export
portalQuery <- function(query) {
  sqlQuery(query, "portaldb")
}

#' Function for queries to index
#' @export
indexQuery <- function(query) {
  sqlQuery(query, "indexdb")
}

#' Function for queries to budget
#' @export
budgetQuery <- function(query) {
  sqlQuery(query, "budgetdb")
}
