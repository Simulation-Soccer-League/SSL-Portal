box::use(
  dplyr,
  lubridate[now, with_tz],
  stringr[str_to_upper, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
)

#' @export

logUpdate <- function(uid, pid, updates){
  portalQuery(
    paste(
      "INSERT INTO updatehistory ( uid, pid, time, attribute, old, new )
                VALUES
                    ",
      paste(
        "(",
        paste(
          uid,
          pid,
          paste0("'", now() |> with_tz("US/Pacific") |> as.numeric(), "'"),
          paste0("'", updates$attribute |> str_to_upper(), "'"),
          updates$old,
          updates$new,
          sep = ","
        ),
        ")",
        collapse = ","
      ),
      ";"
    )
  )
}

#' @export
logRedist <- function(pid){
  portalQuery(
    paste(
      "UPDATE playerdata
      SET redistused = 1 
      WHERE pid =", pid
    )
  )
}

#' @export
logReroll <- function(pid){
  portalQuery(
    paste(
      "UPDATE playerdata
      SET rerollused = 1 
      WHERE pid =", pid
    )
  )
}
