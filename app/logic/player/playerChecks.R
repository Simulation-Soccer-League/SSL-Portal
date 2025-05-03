box::use(
  dplyr,
  purrr[is_empty],
  stringr[str_extract, str_remove_all, str_to_title],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/get[getActivePlayer],
)

#' @export
eligibleReroll <- function(data){
  class <- data$class |> 
    str_extract(pattern = "[0-9]+") |> 
    as.numeric()
  
  (class > (constant$currentSeason$season - 2)) | 
    (data$rerollused == 0)
}

#' @export
eligibleRedist <- function(data){
  class <- data$class |> 
    str_extract(pattern = "[0-9]+") |> 
    as.numeric()
  
  (class > (constant$currentSeason$season - 1)) | 
    (data$redistused == 0)
}

#' @export
updateSummary <- function(current, inputs){
  updates <- 
    dplyr$tibble(
      attribute = 
        current |> 
        dplyr$select(acceleration:throwing) |> 
        # select(!where(is.na)) |> 
        colnames() |>
        str_to_title(),
      old = current |> 
        dplyr$select(acceleration:throwing) |> 
        # select(!where(is.na)) |> 
        t() |> 
        c(),
      new = 
        attribute |>
        str_remove_all(pattern = " ") |> 
        sapply(
          X = _,
          FUN = function(x) {
            if(inputs[[x]] |> is.null()){
              5
            } else {
              inputs[[x]]
            }
          },
          simplify = TRUE
        ) |> 
        unlist()
    ) |> 
    dplyr$mutate(
      old = dplyr$if_else(old |> is.na(), 5, old)
    ) |> 
    dplyr$filter(old != new) 
  
  
  return(updates)
}

#' @export
hasActivePlayer <- function(uid){
  actives <- getActivePlayer(uid)
  
  !is_empty(actives)
}