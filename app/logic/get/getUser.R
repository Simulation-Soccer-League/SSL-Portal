# nolint: line_length_linter
box::use(
  cachem[cache_mem],
  dplyr[across, if_else, mutate, where],
  lubridate,
  memoise[memoise],
  shiny,
  shiny.router[route_link],
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
  memoisedPortalQuery(
    "SELECT
        *
     FROM userInformation
     WHERE uid = {uid};",
    uid = uid
  )
}

#' @export
getUserAwards <- function(uid) {
  memoisedPortalQuery(
    "SELECT
        *
     FROM userAwards
     WHERE uid = {uid};",
    uid = uid
  )
}

#' @export
getUserPlayers <- function(uid) {
  memoisedPortalQuery(
    "SELECT
        name, pid, class, pos_gk
     FROM allplayersview
     WHERE uid = {uid};",
    uid = uid
  )
}

#' @export
linkUser <- function(username, uid, ...) {
  shiny$a(
    href = route_link(paste0("tracker/user?uid=", uid)),
    sprintf("@%s", username),
    ...
  )
}
