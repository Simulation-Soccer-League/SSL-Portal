## Function for queries to mybb
box::use(
  DBI,
  dplyr,
  glue,
  lubridate[now, with_tz],
  RMySQL,
)

dbHost <- Sys.getenv("HOST")
dbPort <- Sys.getenv("PORT") |> as.integer()
dbUser <- Sys.getenv("DBUSER")
dbPassword <- Sys.getenv("DBPASSWORD")

#' @export
createConnection <- function(schema) {
  DBI$dbConnect(
    RMySQL$MySQL(),
    dbname = Sys.getenv(schema),
    host = dbHost,
    port = dbPort,
    user = dbUser,
    password = dbPassword
  )
}

getQuery <- function(query, ..., schema) {
  tryCatch({ 
    con <- createConnection(schema) 
    
    DBI$dbSendQuery(con, "SET NAMES utf8mb4;")
    DBI$dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
    DBI$dbSendQuery(con, "SET character_set_connection=utf8mb4;")
    
    safeQuery <- glue$glue_sql(
      query,
      .con = con,
      .envir = list(...) |> list2env(),
      .na    = "NULL" 
    )
    
    # safeQuery <- DBI$sqlInterpolate(con, query, ...)
    
    req <- DBI$dbGetQuery(con, safeQuery) |> 
      suppressWarnings()
    
    return(req)
  }, error = function(e) {
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop()
  }, finally = {
    # Ensure the connection is closed
    if (!is.null(con) && DBI$dbIsValid(con)) {
      DBI$dbDisconnect(con)
    }
  })
}

setQuery <- function(query, ..., schema) {
  tryCatch({
    con <- createConnection(schema)

    DBI$dbSendQuery(con, "SET NAMES utf8mb4;")
    DBI$dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
    DBI$dbSendQuery(con, "SET character_set_connection=utf8mb4;")
    
    safeQuery <- glue$glue_sql(
      query,
      .con = con,
      .envir = list(...) |> list2env(),
      .na    = "NULL" 
    )
    
    # safeQuery <- DBI$sqlInterpolate(con, query, ...)
    
    req <- DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
    
    return(req)
  }, error = function(e) {
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop(e$message)
  }, finally = {
    # Ensure the connection is closed
    if (!is.null(con) && DBI$dbIsValid(con)) {
      DBI$dbDisconnect(con)
    }
  })
}



#' Function for queries to mybb
#' @export
mybbQuery <- function(query, ..., type = "get") {
  if (type == "get") {
    getQuery(query, ..., schema = "mybb")    
  } else {
    setQuery(query, ..., schema = "mybb")    
  }
}

#' Function for queries to portal
#' @export
portalQuery <- function(query, ..., type = "get") {
  if (type == "get") {
    getQuery(query, ..., schema = "portal")    
  } else {
    setQuery(query, ..., schema = "portal")    
  }
}

#' Function for queries to index
#' @export
indexQuery <- function(query, ..., type = "get") {
  if (type == "get") {
    getQuery(query, ..., schema = "index")    
  } else {
    setQuery(query, ..., schema = "index")    
  }
}

#' Function for queries to budget
#' @export
budgetQuery <- function(query, ..., type = "get") {
  if (type == "get") {
    getQuery(query, ..., schema = "budget")    
  } else {
    setQuery(query, ..., schema = "budget")    
  }
}
