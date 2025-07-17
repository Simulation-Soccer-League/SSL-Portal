box::use(
  dplyr,
  lubridate[as_date, floor_date, force_tz, now, with_tz],
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
  tidyr[pivot_longer, pivot_wider],
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
updateSummary <- function(current, input, type = "update"){
  summary <- tibble(tpe = current$tpe)
  
  ## Adds player information, traits and positions
  if(type %in% c("redistribution", "reroll")){
    summary <- 
      tibble(
        first = str_trim(input$firstName), # Remember to add replace ' with \\\\' before SQL
        last = str_trim(input$lastName),
        birthplace = input$birthplace,
        nationality = input$nationality,
        height = input$height,
        weight = input$weight,
        hair_color = input$hairColor,
        hair_length = input$hairLength,
        skintone = input$skintone,
        render = input$render,
        `left foot` = 
          dplyr$if_else(
            input$footedness == "Right", 
            max(current$`left foot`, 10), 
            20
          ),
        `right foot` = dplyr$if_else(
          input$footedness == "Right", 
          20, 
          max(current$`right foot`, 10)
        ),
      )
    
    if(input$playerType == "Outfield"){
      summary$position <- input$primary[1]
      
      # Add pos_ variables for each position
      positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
      for (pos in positions) {
        summary <- summary |> 
          dplyr$mutate(
            !!paste0("pos_", str_to_lower(pos)) := 
              dplyr$case_when(
                pos %in% input$primary ~ 20,
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
  }
  
  # Add attributes variables and their value
  for (att in (constant$attributes$attribute)) {
    summary <- summary |> 
      dplyr$mutate(
        !!str_to_lower(att) := 
          input[[att |> str_remove_all(" ")]]
      )
  }
  
  ## This takes all inputs and checks for differences against the current build
  # summary <- 
  summary |> 
    dplyr$add_row(
      current |> 
        dplyr$select(colnames(summary))
    ) |> 
    dplyr$mutate(source = c("new", "old")) |> 
    pivot_longer(
      cols = !source,
      values_transform = as.character
    ) |> 
    pivot_wider(
      names_from = source,
      values_from = value
    ) |>  
    dplyr$filter(old != new) |> 
    dplyr$select(attribute = name, old, new)
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
    query = "
      SELECT *
      FROM playerdata
      WHERE first = {first}
        AND last  = {last};
    ",
    first = first,
    last  = last
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
  for (att in (constant$attributes$attribute)) {
    summary <- summary  |> 
      dplyr$mutate(
        !!str_to_lower(att) := 
          input[[att |> str_remove_all(" ")]]
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
      first = str_trim(input$firstName),
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
              pos %in% input$primary ~ 20,
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
        .cols = !render,
        ~ dplyr$if_else(.x == "", NA, .x)
      )
    )
  
  # Insert to database
  # grab field names and the first row of data
  fields <- colnames(summary)
  values <- as.list(summary[1, , drop = FALSE])
  names(values) <- fields
  
  # quote the column identifiers for SQL
  cols_sql <- paste0("`", fields, "`", collapse = ", ")
  # build a matching list of ?placeholders
  placeholders <- paste0(paste0("{", fields, "}"), collapse = ", ")
  
  # one tidy SQL string
  sql <- sprintf(
    "INSERT INTO playerdata (%s) VALUES (%s);",
    cols_sql,
    placeholders
  )
  
  # call portalQuery with query + named params
  do.call(
    portalQuery,
    c(list(query = sql, type = "set"), values)
  )
  
}

#' @export
checkApprovingPlayer <- function(uid) {
  portalQuery(
    query = "
      SELECT *
      FROM playerdata
      WHERE uid      = {uid}
        AND status_p = -1;
    ",
    uid = uid
  ) |>
    nrow() > 0
}

#' @export
completedAC <- function(pid) {
  weekStart <- now() |>
    with_tz("US/Pacific") |>
    floor_date("week", week_start = "Monday") |>
    as.numeric()
  
  portalQuery(
    query = "
      SELECT *
      FROM tpehistory
      WHERE pid    = {pid}
        AND source LIKE '%Activity Check'
        AND time   > {weekStart};
    ",
    pid       = pid,
    weekStart = weekStart
  ) |>
    nrow() > 0
}

#' @export
completedTC <- function(pid) {
  start <- constant$currentSeason$startDate |>
    as_date() |>
    force_tz("US/Pacific") |>
    as.numeric()
  
  portalQuery(
    query = "
      SELECT *
      FROM tpehistory
      WHERE pid    = {pid}
        AND source LIKE '%Training Camp'
        AND time   > {start};
    ",
    pid   = pid,
    start = start
  ) |> 
    nrow() > 0
}
