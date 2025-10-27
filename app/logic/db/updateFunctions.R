box::use(
  dplyr,
  lubridate[as_date, now, with_tz],
  purrr[pwalk],
  stringr[
    str_detect,
    str_to_lower, 
    str_to_upper, 
  ],
  tidyr[pivot_longer]
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/discord[sendApprovedCreate, sendRetiredPlayer],
  app/logic/db/get[getActivePlayer,],
  app/logic/db/logFunctions[logBankTransaction],
  app/logic/db/updateFunctions[updateTPE],
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
            newValue  = updates$new
          ),
        .f = 
          function(col, newValue) {
            # quote the column name exactly once
            colQ <- paste0("`", str_to_lower(col), "`")
            
            portalQuery(
              query = paste0(
                "UPDATE playerdata\n",
                "SET ", colQ, " = {newValue}\n",
                "WHERE pid = {pid};"
              ),
              newValue = newValue,
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
updateTPE <- function(uid, pids, tpe) {
  
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
        .l = list(pid = pids, source = tpe$source, tpeVal = tpe$tpe),
        .f = 
          function(pid, source, tpeVal) {
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
              tpe    = tpeVal,
              type = "set"
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
          }
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
approveTransaction <- function(data, uid) {
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for (i in seq_len(nrow(data))) {
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
rejectTransaction <- function(data, uid) {
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    for (i in seq_len(nrow(data))) {
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

#' @export
approvePlayer <- function(data, uid) {
  
  currentTime <- now() |> 
    with_tz("US/Pacific") |> 
    as.numeric()
  
  # Begin the transaction
  portalQuery(
    query = "START TRANSACTION;",
    type = "set"
  )
  
  tryCatch({
    
    ## Adding initial TPE to history
    portalQuery(
      query = "INSERT INTO tpehistory (time, uid, pid, source, tpe)
               SELECT {currentTime}, 1, pid, 'Initial TPE', {startTPE}
               FROM playerdata
               WHERE pid = {pid};",
      currentTime = currentTime,
      pid = data$pid,
      startTPE = constant$startingTPE,
      type = "set"
    )
    
    ## Getting and logging initial attribute changes
    attributes <- 
      portalQuery(
        "SELECT pid, 
          `pos_st`, `pos_lam`, `pos_cam`, `pos_ram`, `pos_lm`, `pos_cm`, `pos_rm`, 
          `pos_lwb`, `pos_cdm`, `pos_rwb`, `pos_ld`, `pos_cd`, `pos_rd`, `pos_gk`, 
          acceleration, agility, balance, `jumping reach`, `natural fitness`, pace, 
          stamina, strength, corners, crossing, dribbling, finishing, `first touch`, 
          `free kick`, heading, `long shots`, `long throws`, marking, passing, 
          `penalty taking`, tackling, technique, aggression, anticipation, bravery, 
          composure, concentration, decisions, determination, flair, leadership, 
          `off the ball`, positioning, teamwork, vision, `work rate`, `aerial reach`, 
          `command of area`, communication, eccentricity, handling, kicking, 
          `one on ones`, reflexes, `tendency to rush`, `tendency to punch`, throwing, 
          traits, `left foot`, `right foot`
        FROM allplayersview
        WHERE pid = {pid};",
      pid = data$pid
    )
    
    attrLong <- attributes |> 
      pivot_longer(!pid, values_transform = as.character)
    
    pwalk(
      .l = list(
        attribute = attrLong$name,
        new       = attrLong$value
      ),
      .f = 
        function(attribute, new) {
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
                  1,
                  {pid},
                  {time},
                  {attribute},
                  {old},
                  {new}
                );",
            pid       = data$pid,
            time      = currentTime,
            attribute = attribute |> str_to_upper(),
            old       = 
              dplyr$if_else(
                str_detect(attribute, "pos_"),
                "0",
                dplyr$if_else(
                  attribute == "traits",
                  "NO TRAITS",
                  "5"
                )
              ),
            new       = new,
            type = "set"
          )
        }
    )
    
    ## Adding Academy Contract to bank history
    logBankTransaction(
      uid = uid, 
      pid = data$pid, 
      source = "Academy Contract",
      transaction = 3000000,
      status = 1
    )
    
    ## Handling Catchup-TPE
    today <- now() |> 
      with_tz("US/Pacific") |> 
      as_date() |> 
      as.numeric()
    
    seasonStart <- constant$currentSeason$startDate |> 
      as_date() |> 
      as.numeric()
    
    tpe <- 
      dplyr$tibble(
        source = "Catch-up TPE",
        tpe = floor((today - seasonStart) / 7) * 6
      )
    
    if (tpe$tpe > 0) {
      updateTPE(uid = 1, pids = data$pid, tpe = tpe)
    }
    
    sendApprovedCreate(data = data)
    
    ## Final update to `playerdata`
    class <- constant$currentSeason$season + 1
    
    portalQuery(
      "UPDATE playerdata
      SET rerollused = 0,
          redistused = 0,
          team = -1,
          affiliate = 1,
          status_p = 1,
          name = concat(trim(first), ' ', trim(last)),
          class = concat('S', {class}),
          created = {time}
      WHERE pid = {pid};",
      class = class,
      time = currentTime,
      pid = data$pid,
      type = "set"
    )
    
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
  
    message("Error approving player, transaction rolled back: ", e$message)
    
    stop()
  })
  
}

#' @export
retirePlayer <- function(data, uid) {
  
  result <- tryCatch({
    # Begin the transaction
    portalQuery(
      query = "START TRANSACTION;",
      type = "set"
    )
    
    portalQuery(
      query = "
              INSERT INTO updatehistory (time, uid, pid, attribute, old, new)
              VALUES (UNIX_TIMESTAMP(), {uid}, {pid}, 'Player Status', 'Active', 'Retiring');
            ",
      uid = uid,
      pid = getActivePlayer(uid),
      type = "set"
    )
    
    portalQuery(
      query = "UPDATE playerdata 
            SET status_p = 2 
            WHERE pid = {pid};",
      pid = getActivePlayer(uid),
      type = "set"
    )
    
    sendRetiredPlayer(data)
    
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
