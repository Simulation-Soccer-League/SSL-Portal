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

#' Function for logging (multiple) bank transactions
#' @export
logBankTransaction <- function(uid, data, status = 1) {
  con <- createConnection("portal")
  
  timestamp <- now() |> 
    with_tz("US/Pacific") |> 
    as.numeric()
  
  DBI$dbExecute(con, "SET NAMES utf8mb4;")
  DBI$dbExecute(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbExecute(con, "SET character_set_connection=utf8mb4;")
  
  DBI$dbExecute(con, "START TRANSACTION;")
  
  tryCatch({

    insert <- "INSERT INTO banktransactions (
    time, pid, `source`, `transaction`, `status`, uid
          ) VALUES "
    
    values <- 
      glue$glue_sql(
        "({time}, {pid}, {source}, {transaction}, {status}, {uid})",
        .con = con,
        time        = timestamp,
        pid         = data$pid,
        source      = data$source,
        transaction = data$amount,
        status      = status,
        uid         = uid
      ) |> 
        glue$glue_sql_collapse(sep = ", ")
      
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    # safeQuery <- DBI$sqlInterpolate(con, query, ...)
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
    
    DBI$dbExecute(con, "COMMIT;")
    
  }, error = function(e) {
    DBI$dbExecute(con, "ROLLBACK;")
    
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop(e$message)
  }, finally = {
    # Ensure the connection is closed
    DBI$dbDisconnect(con)
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
