box::use(
  dplyr,
  stringr[str_to_lower, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
)


#' @export
updatePlayerData <- function(con, pid, updates, bankedTPE = NULL) {
  # con        : DBI connection (needed to quote identifiers)
  # pid        : player id
  # updates    : data.frame/tibble with columns 'attribute' and 'new'
  # bankedTPE  : optional single numeric
  
  # 1) update each attribute in its own parameterized call
  pwalk(
    list(
      col       = updates$attribute,
      new_value = updates$new
    ),
    function(col, new_value) {
      # quote the column name exactly once
      col_q <- DBI$dbQuoteIdentifier(con, str_to_lower(col))
      
      portalQuery(
        query = paste0(
          "UPDATE playerdata\n",
          "SET ", col_q, " = ?new_value\n",
          "WHERE pid = ?pid;"
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
      query = "
        UPDATE playerdata
        SET tpebank = tpebank + ?bankedTPE
        WHERE pid = ?pid;
      ",
      bankedTPE = bankedTPE,
      pid       = pid,
      type = "set"
    )
  }
}

#' @export
updateTPE <- function(pid, tpe){
  portalQuery(
    query = "
      UPDATE playerdata
      SET
        tpe      = tpe + ?tpe,
        tpebank  = tpebank + ?tpe
      WHERE pid = ?pid;
    ",
    tpe = tpe$tpe,
    pid = pid,
    type = "set"
  )
}