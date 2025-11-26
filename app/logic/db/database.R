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
  
  DBI$dbBegin(con)
  
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
    
    DBI$dbCommit(con)
    
  }, error = function(e) {
    DBI$dbRollback(con)
    
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop(e$message)
  }, finally = {
    # Ensure the connection is closed
    DBI$dbDisconnect(con)
  })
  
}

#' Function for updating and logging (multiple) TPE earnings
#' @export
updateTPE <- function(uid, tpeData) {
  con <- createConnection("portal")
  
  timestamp <- now() |> 
    with_tz("US/Pacific") |> 
    as.numeric()
  
  DBI$dbExecute(con, "SET NAMES utf8mb4;")
  DBI$dbExecute(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbExecute(con, "SET character_set_connection=utf8mb4;")
  
  DBI$dbBegin(con)
    
  tryCatch({
    
    # Logs TPE history
    insert <- "INSERT INTO tpehistory (
      `uid`, `pid`, `time`, `source`, `tpe`
          ) VALUES "
    
    values <- 
      glue$glue_sql(
        "({uid}, {pid}, {time}, {source}, {tpe})",
        .con = con,
        time        = timestamp,
        pid         = tpeData$pid,
        source      = tpeData$source,
        tpe         = tpeData$tpe,
        uid         = uid
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
    
    # Updates TPE for players
    DBI$dbExecute(con, "DROP TEMPORARY TABLE IF EXISTS temp_updates;")
    DBI$dbExecute(con, "CREATE TEMPORARY TABLE temp_updates (pid INT NOT NULL, tpe INT NOT NULL);")
    
    
    insert <- "INSERT INTO temp_updates (pid, tpe) VALUES "
    
    values <- 
      glue$glue_sql(
        "({pid}, {tpe})",
        .con = con,
        pid = tpeData$pid,
        tpe = tpeData$tpe
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery)
  
    DBI$dbExecute(con, 
    "UPDATE playerdata p
     JOIN temp_updates u ON p.pid = u.pid
     SET 
        p.tpe      = p.tpe + u.tpe,
        p.tpebank  = p.tpebank + u.tpe;"
    )
    
    DBI$dbCommit(con)
    
  }, error = function(e) {
    DBI$dbRollback(con)
    
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
