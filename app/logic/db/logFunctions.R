box::use(
  dplyr,
  lubridate[now, with_tz],
  purrr[pwalk],
  stringr[str_to_upper, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
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
              ?uid,
              ?pid,
              ?time,
              ?attribute,
              ?old,
              ?new
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
logRedist <- function(pid){
  portalQuery(
    "UPDATE playerdata
    SET redistused = 1 
    WHERE pid = ?pid;", 
    pid = pid,
    type = "set"
  )
}

#' @export
logReroll <- function(pid){
  portalQuery(
    "UPDATE playerdata
    SET rerollused = 1 
    WHERE pid = ?pid;", 
    pid = pid,
    type = "set"
  )
}

#' @export
#' HAS BEEN MOVED TO updateTPE()
logTPE <- function(uid, pid, tpe) {
  # one timestamp
  ts <- 
    now() |>
    with_tz("US/Pacific") |> 
    as.numeric()
  
  # fire one parameterized INSERT per row
  pwalk(
    .l = list(source = tpe$source, tpe_val = tpe$tpe),
    .f = 
      function(source, tpe_val) {
        portalQuery(
          query = "
            INSERT INTO tpehistory (
              uid, pid, time, source, tpe
            ) VALUES (
              ?uid, ?pid, ?time, ?source, ?tpe
            );
          ",
          uid    = uid,
          pid    = pid,
          time   = ts,
          source = source,
          tpe    = tpe_val,
          type = "set"
        )
      }
  )
}

#' @export
logBankTransaction <- function(uid, pid, source, transaction, status = 1){
  ts <- 
    now() |>
    with_tz("US/Pacific") |> 
    as.numeric()
  
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  # Try executing all inserts; if an error occurs, rollback the transaction.
  tryCatch({
    n <- length(pid)  # Assume all vectors have equal length
    
    for(i in seq_len(n)){
      res <- portalQuery(
        query = 
          "INSERT INTO banktransactions (time, pid, `source`, `transaction`, `status`, uid) 
         VALUES (?time, ?pid, ?source, ?transaction, ?status, ?uid);",
        time        = ts,
        pid         = pid[i],
        source      = source[i],
        transaction = transaction[i],
        status      = status,
        uid         = uid,
        type        = "set"
      )
      
      # Optionally check if the insert failed (depends on how portalQuery returns errors)
      # If portalQuery returns NULL or a specific error code, you could trigger an error:
      if (is.null(res)) {
        stop("Insert failed for row ", i)
      }
    }
    
    # All rows inserted successfully; commit the transaction
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # An error occurred; rollback the transaction
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Transaction failed, rolling back: ", e$message)
  })
  
}
