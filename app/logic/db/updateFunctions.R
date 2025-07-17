box::use(
  DBI,
  dplyr,
  lubridate[now, with_tz],
  purrr[pwalk],
  stringr[str_to_lower, str_to_upper, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
)


#' @export
updatePlayerData <- function(uid, pid, updates, bankedTPE = NULL) {
  # uid        : user id
  # pid        : player id
  # updates    : data.frame/tibble with columns 'attribute' and 'new'
  # bankedTPE  : optional single numeric
  
  result <- 
    tryCatch({
      # Start transaction to ensure all updates are applied atomically.
      portalQuery(query = "START TRANSACTION;", type = "set")
      
      # one timestamp
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
              query = 
                "INSERT INTO updatehistory (
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
                );",
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
      
      # 1) update each attribute in its own parameterized call
      pwalk(
        .l = 
          list(
            col       = updates$attribute,
            new_value = updates$new
          ),
        .f = 
          function(col, new_value) {
            # quote the column name exactly once
            col_q <- paste0("`", str_to_lower(col), "`")
            
            portalQuery(
              query = paste0(
                "UPDATE playerdata\n",
                "SET ", col_q, " = {new_value}\n",
                "WHERE pid = {pid};"
              ),
              new_value = new_value,
              pid       = pid,
              type = "set"
            )
          }
      )
      
      # 2) optionally bump tpebank in one more call
      if (!is.null(bankedTPE)) {
        portalQuery(
          query =
            "UPDATE playerdata
            SET tpebank = {bankedTPE}
            WHERE pid = {pid};",
          bankedTPE = bankedTPE,
          pid       = pid,
          type = "set"
        )
      }
      
      TRUE  # Indicate success if all insertions succeed.
    }, error = function(e) {
      # If any error occurs, rollback the transaction and show an error message.
      portalQuery(query = "ROLLBACK;", type = "set")
      
      message("Error executing query: ", e)
      
      FALSE
    })
  
  # If the tryCatch block completed successfully, commit the transaction.
  if (result) {
    portalQuery(query = "COMMIT;", type = "set")
  } else {
    stop()
  }
}

#' @export
updateTPE <- function(uid, pid, tpe){
  
  result <- 
    tryCatch({
      # Start transaction to ensure all updates are applied atomically.
      portalQuery(query = "START TRANSACTION;", type = "set")
      
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
              query = 
                "INSERT INTO tpehistory (
                    uid, pid, time, source, tpe
                  ) VALUES (
                    {uid}, {pid}, {time}, {source}, {tpe}
                  );",
              uid    = uid,
              pid    = pid,
              time   = ts,
              source = source,
              tpe    = tpe_val,
              type = "set"
            )
          }
      )
      
      portalQuery(
        query = 
          "UPDATE playerdata
            SET
              tpe      = tpe + {tpe},
              tpebank  = tpebank + {tpe}
            WHERE pid = {pid};",
        tpe = tpe$tpe,
        pid = pid,
        type = "set"
      )
      
      TRUE  # Indicate success if all insertions succeed.
    }, error = function(e) {
      # If any error occurs, rollback the transaction and show an error message.
      portalQuery(query = "ROLLBACK;", type = "set")
      
      message("Error executing query: ", e)

      FALSE
    })
  
  # If the tryCatch block completed successfully, commit the transaction.
  if (result) {
    portalQuery(query = "COMMIT;", type = "set")
  } else {
    stop()
  }
  
}

#' @export
approveTransaction <- function(data, uid){
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for (i in 1:nrow(data)) {
      portalQuery(
        query = "UPDATE banktransactions 
               SET status = 1, approvedBy = {approvedBy}
               WHERE status = 0 
                 AND time = {time} 
                 AND pid = {pid} 
                 AND source = {source};",
        approvedBy = uid,
        time       = data[i, "Time"],
        pid        = data[i, "pid"],
        source     = data[i, "Source"],
        type       = "set"
      )
    }
    
    # Commit the transaction if all updates succeed
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # Rollback the transaction if any error occurs
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Error updating banktransactions, transaction rolled back: ", e$message)
  })
  
}

#' @export
rejectTransaction <- function(data, uid){
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for (i in 1:nrow(data)) {
      portalQuery(
        query = "UPDATE banktransactions 
               SET status = -1, approvedBy = {approvedBy}
               WHERE status = 0 
                 AND time = {time} 
                 AND pid = {pid} 
                 AND source = {source};",
        approvedBy = uid,
        time       = data[i, "Time"],
        pid        = data[i, "pid"],
        source     = data[i, "Source"],
        type       = "set"
      )
    }
    
    # Commit the transaction if all updates succeed
    portalQuery(
      query = "COMMIT;",
      type = "set"
    )
    
  }, error = function(e) {
    # Rollback the transaction if any error occurs
    portalQuery(
      query = "ROLLBACK;",
      type = "set"
    )
    message("Error updating banktransactions, transaction rolled back: ", e$message)
  })
  
}
