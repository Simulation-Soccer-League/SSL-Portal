box::use(
  DBI,
  dplyr,
  glue,
  lubridate[now, with_tz],
  purrr[pwalk],
  stringr[
    str_to_upper, 
  ],
)

box::use(
  app/logic/db/database[
    createConnection,
    portalQuery,
  ],
)

#' @export
#' MOVED TO updatePlayerData()
logUpdate <- function(uid, pid, updates) {
  # compute a single timestamp in US/Pacific once
  ts <- 
    now() |> 
    with_tz("US/Pacific") |>  
    as.numeric()
  
  # uppercase the attribute column
  updates <- 
    updates |>  
    dplyr$mutate(attribute = str_to_upper(attribute))
  
  # for each row, fire a parameterized insert
  pwalk(
    .f = 
      function(attribute, old, new) {
        portalQuery(
          query = "
            INSERT INTO updatehistory (
              uid,
              pid,
              time,
              attribute,
              old,
              new
            ) VALUES (
              {uid},
              {pid},
              {time},
              {attribute},
              {old},
              {new}
            );
          ",
          uid       = uid,
          pid       = pid,
          time      = ts,
          attribute = attribute,
          old       = old,
          new       = new,
          type = "set"
        )
      },
    .l = list(attribute = updates$attribute,
              old       = updates$old,
              new       = updates$new)
  )
  
  
}

#' @export
logRedist <- function(pid) {
  portalQuery(
    "UPDATE playerdata
    SET redistused = 1 
    WHERE pid = {pid};", 
    pid = pid,
    type = "set"
  )
}

#' @export
logReroll <- function(pid) {
  portalQuery(
    "UPDATE playerdata
    SET rerollused = 1 
    WHERE pid = {pid};", 
    pid = pid,
    type = "set"
  )
}

#' HAS BEEN MOVED TO updateTPE()
logTPE <- function(uid, pid, tpe) {
  # one timestamp
  ts <- 
    now() |>
    with_tz("US/Pacific") |> 
    as.numeric()
  
  # fire one parameterized INSERT per row
  pwalk(
    .l = list(source = tpe$source, tpeVal = tpe$tpe),
    .f = 
      function(source, tpeVal) {
        portalQuery(
          query = "
            INSERT INTO tpehistory (
              uid, pid, time, source, tpe
            ) VALUES (
              {uid}, {pid}, {time}, {source}, {tpe}
            );
          ",
          uid    = uid,
          pid    = pid,
          time   = ts,
          source = source,
          tpe    = tpeVal,
          type = "set"
        )
      }
  )
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
