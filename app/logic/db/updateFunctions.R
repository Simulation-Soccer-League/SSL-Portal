box::use(
  dplyr,
  stringr[str_to_lower, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
)


#' @export
updatePlayerData <- function(pid, updates, bankedTPE = NULL){
  portalQuery(
    paste(
      "UPDATE playerdata
               SET",
      paste(
        paste("`", str_to_lower(updates$attribute), "`", sep = ""),
        "=",
        updates$new,
        collapse = ", "
      ), 
      dplyr$if_else(bankedTPE |> is.null(), "", paste(", tpebank = ", bankedTPE)),
      "WHERE pid =", pid,
      ";"
    )
  )
}