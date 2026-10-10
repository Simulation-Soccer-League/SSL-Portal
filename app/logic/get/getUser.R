# nolint: line_length_linter
box::use(
  cachem[cache_mem],
  dplyr[across, if_else, mutate, where],
  lubridate,
  memoise[memoise],
  tidyr[replace_na],
)

box::use(
  app / logic / db / database[indexQuery, portalQuery,],
)

# Alternative memoised function for heavy calls
memoisedPortalQuery <- 
  memoise(
    portalQuery,
    cache = cache_mem(max_age = 60*10)
  )


#' @export
getUserInformation <- function(uid) {
  portalQuery(
    "SELECT
        *
     FROM userInformation
     WHERE uid = {uid};",
    uid = uid
  )
}

#' @export
getUserAwards <- function(uid) {
  portalQuery(
    "SELECT
        *
     FROM userAwards
     WHERE uid = {uid};",
    uid = uid
  )
}

#' @export
getUserPlayers <- function(uid) {
  portalQuery(
    "SELECT
        name, pid, class, pos_gk
     FROM allplayersview
     WHERE uid = {uid};",
    uid = uid
  )
}