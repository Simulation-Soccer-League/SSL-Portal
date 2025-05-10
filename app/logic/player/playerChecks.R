box::use(
  dplyr,
  purrr[is_empty],
  reactable[reactable],
  rlang[`!!`, `:=`],
  shiny,
  stringr[
    str_extract, 
    str_remove_all, 
    str_replace_all, 
    str_to_lower,
    str_to_title, 
    str_trim
    ],
  tibble[tibble],
  tidyr[pivot_longer],
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

#' @export
checkDuplicatedNames <- function(first, last){
  first <- str_trim(first)
  last <- str_trim(last)
  
  portalQuery(
    paste(
      "SELECT * FROM playerdata WHERE first = '", first, "' AND last = '", last, "';", sep = ""
    )
  ) |> 
    nrow() > 0
}

#' @export
verifyBuild <- function(input, bankedTPE, session){
  summary <- 
    tibble(
      first = str_trim(input$firstName), # Remember to add replace ' with \\\\' before SQL
      last = str_trim(input$lastName),
      tpe = 350,
      tpebank = bankedTPE(),
      birthplace = input$birthplace,
      nationality = input$nationality,
      height = input$height,
      weight = input$weight,
      hair_color = input$hairColor,
      hair_length = input$hairLength,
      skintone = input$skintone,
      render = input$render,
      `left foot` = dplyr$if_else(input$footedness == "Right", 10, 20),
      `right foot` = dplyr$if_else(input$footedness == "Right", 20, 10),
    )
  
  if(input$playerType == "Outfield"){
    summary$position <- input$primary
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      summary <- summary |> 
        dplyr$mutate(
          !!paste0("pos_", str_to_lower(pos)) := 
            dplyr$case_when(
              input$primary == pos ~ 20,
              pos %in% input$secondary ~ 15,
              TRUE ~ 0
            )
        )
    }
    
    summary$traits <- paste0(input$traits, collapse = constant$traitSep)
  } else {
    summary$position <- "GK"
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      summary <- summary |> 
        dplyr$mutate(
          !!paste0("pos_", str_to_lower(pos)) := 
            dplyr$case_when(
              pos == "GK" ~ 20,
              TRUE ~ 0
            )
        )
    }
  }
  
  # Add attributes variables for each position
  for (att in (constant$attributes$attribute |> str_remove_all(" "))) {
    summary <- summary  |> 
      dplyr$mutate(
        !!str_to_lower(att) := 
          input[[att]]
      )
  }
  
  shiny$showModal(
    shiny$modalDialog(
      shiny$span("Check the information you have submitted.", style = "color: red;"),
      shiny$br(),
      summary |> 
        pivot_longer(
          dplyr$everything(),
          names_to = "Property",
          values_to = "Value",
          values_transform = as.character
        ) |> 
        reactable(
          pagination = FALSE
        ),
      shiny$br(),
      title = "Verify your build!",
      footer = 
        shiny$tagList(
          shiny$modalButton("Go back"),
          shiny$actionButton(
            inputId = session$ns("confirmBuild"),
            label = "Submit build"
          )
        ),
      easyClose = FALSE
    )
  )
  
}

#' @export
submitBuild <- function(input, bankedTPE, userinfo){
  summary <- 
    tibble(
      uid = userinfo$uid,
      status_p = -1,
      first = str_trim(input$firstName) |> 
        str_replace_all("'", "\\\\'"), # Remember to add replace ' with \\\\' before SQL
      last = str_trim(input$lastName) |> 
        str_replace_all("'", "\\\\'"),
      tpe = 350,
      tpebank = bankedTPE(),
      birthplace = input$birthplace |> 
        str_replace_all("'", "\\\\'"),
      nationality = input$nationality,
      height = input$height,
      weight = input$weight,
      hair_color = input$hairColor,
      hair_length = input$hairLength,
      skintone = input$skintone,
      render = input$render |> 
        str_replace_all("'", "\\\\'"),
      `left foot` = dplyr$if_else(input$footedness == "Right", 10, 20),
      `right foot` = dplyr$if_else(input$footedness == "Right", 20, 10),
    )
  
  if(input$playerType == "Outfield"){
    summary$position <- input$primary
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      summary <- summary |> 
        dplyr$mutate(
          !!paste0("pos_", str_to_lower(pos)) := 
            dplyr$case_when(
              input$primary == pos ~ 20,
              pos %in% input$secondary ~ 15,
              TRUE ~ 0
            )
        )
    }
    
    summary$traits <- paste0(input$traits, collapse = constant$traitSep)
  } else {
    summary$position <- "GK"
    
    # Add pos_ variables for each position
    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
    for (pos in positions) {
      summary <- summary |> 
        dplyr$mutate(
          !!paste0("pos_", str_to_lower(pos)) := 
            dplyr$case_when(
              pos == "GK" ~ 20,
              TRUE ~ 0
            )
        )
    }
  }
  
  # Add attributes variables for each position
  for (att in (constant$attributes$attribute |> str_to_lower())) {
    summary <- summary  |> 
      dplyr$mutate(
        !!str_to_lower(att) := 
          input[[att |> str_remove_all(" ")]]
      )
  }

  # Adding 'string' to character variables for SQL  
  summary <- 
    summary |> 
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.character),
        ~ paste("'", .x, "'", sep = "")
      ),
      dplyr$across(
        dplyr$everything(),
        ~ dplyr$if_else(.x == "", NA, .x)
      )
    )
  
  # Insert to database
  portalQuery(
    paste(
      "INSERT INTO playerdata (", paste("`", colnames(summary), "`", sep = "", collapse = ", "), ")
      VALUES",
      paste(
        "(",
        paste(
          summary[1, ],
          collapse = ","
        ),
        ")",
        collapse = ","
      ),
      ";"
    )
  )
  
}

#' @export
checkApprovingPlayer <- function(uid){
  portalQuery(
    paste(
      "SELECT * from playerdata WHERE uid = ", uid, "AND status_p = -1;"
    )
  ) |> 
    nrow() > 0
}