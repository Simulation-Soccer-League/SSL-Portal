box::use(
  dplyr,
  lubridate[as_date, floor_date, force_tz, now, with_tz],
  purrr[is_empty],
  reactable[reactable],
  rlang[`!!`, `:=`],
  shiny,
  shinyFeedback[showToast],
  stringr[
    str_extract, 
    str_remove_all, 
    str_replace_all, 
    str_to_lower,
    str_to_title, 
    str_trim
    ],
  tibble[tibble],
  tidyr[pivot_longer, pivot_wider, replace_na],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/discord[sendNewCreate],
  app/logic/db/get[getActivePlayer],
  app/logic/db/login[isNonActiveForumUser],
)

#' @export
eligibleReroll <- function(data){
  class <- data$class |> 
    str_extract(pattern = "[0-9]+") |> 
    as.numeric()
  
  (class > (constant$currentSeason$season - 2)) & 
    (data$rerollused == 0)
}

#' @export
eligibleRedist <- function(data){
  class <- data$class |> 
    str_extract(pattern = "[0-9]+") |> 
    as.numeric()
  
  (class > (constant$currentSeason$season - 1)) & 
    (data$redistused == 0)
}

#' @export
updateSummary <- function(current, input, type = "update"){
  summary <- tibble(tpe = current$tpe)
  
  ## Adds player information, traits and positions
  if(type %in% c("redistribution", "reroll")){
    summary <- 
      tibble(
        first = str_trim(input$firstName), 
        last = str_trim(input$lastName),
        name = paste(first, last),
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
            max(current$`right foot`, 10), 
            20
          ),
        `right foot` = dplyr$if_else(
          input$footedness == "Right", 
          20, 
          max(current$`left foot`, 10)
        ),
        pronouns = input$pronouns |> sort() |> paste0(collapse = constant$traitSep)
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
    value <- input[[att |> str_remove_all(" ")]]
    
    if (value |> is.null()) {
      summary <- summary |> 
        dplyr$mutate(
          !!str_to_lower(att) := 
            5
        )
    } else {
      summary <- summary |> 
        dplyr$mutate(
          !!str_to_lower(att) := 
            value
        )
    }
    
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
    dplyr$mutate(
      dplyr$across(
        dplyr$everything(),
        ~ replace_na(.x, 5)
      )
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
      first = str_trim(input$firstName), 
      last = str_trim(input$lastName),
      tpe = constant$startingTPE,
      tpebank = bankedTPE(),
      birthplace = input$birthplace,
      nationality = input$nationality,
      height = input$height,
      weight = input$weight,
      hair_color = input$hairColor,
      hair_length = input$hairLength,
      skintone = input$skinColor,
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
  
  # Add attributes variables and their value
  for (att in (constant$attributes$attribute)) {
    value <- input[[att |> str_remove_all(" ")]]
    
    if (value |> is.null()) {
      summary <- summary |> 
        dplyr$mutate(
          !!str_to_lower(att) := 
            5
        )
    } else {
      summary <- summary |> 
        dplyr$mutate(
          !!str_to_lower(att) := 
            value
        )
    }
    
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
  
  tryCatch({
    # Begin the transaction
    portalQuery(
      query = "START TRANSACTION;",
      type = "set"
    )
    
    summary <- 
      tibble(
        uid = userinfo$uid,
        status_p = -1,
        first = str_trim(input$firstName),
        last = str_trim(input$lastName),
        tpe = constant$startingTPE,
        tpebank = bankedTPE(),
        birthplace = input$birthplace,
        nationality = input$nationality,
        height = input$height,
        weight = input$weight,
        hair_color = input$hairColor,
        hair_length = input$hairLength,
        skintone = input$skinColor,
        render = input$render,
        `left foot` = dplyr$if_else(input$footedness == "Right", 10, 20),
        `right foot` = dplyr$if_else(input$footedness == "Right", 20, 10),
        pronouns = input$pronouns |> sort() |> paste0(collapse = constant$traitSep)
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
      
      summary$traits <- "NO TRAITS"
    }
    
    # Add attributes variables and their value
    for (att in (constant$attributes$attribute)) {
      value <- input[[att |> str_remove_all(" ")]]
      
      if (value |> is.null()) {
        summary <- summary |> 
          dplyr$mutate(
            !!str_to_lower(att) := 
              5
          )
      } else {
        summary <- summary |> 
          dplyr$mutate(
            !!str_to_lower(att) := 
              value
          )
      }
      
    }
    
    # helper: fetch df[[col]] if it exists, otherwise emit SQL NULL
    getSummaryValue <- function(df, col) {
      if (col %in% names(df)) {
        df[[col]]
      } else {
        NA
      }
    }

    # call portalQuery with query + named params
    portalQuery(
      query =
        "INSERT INTO playerdata (`uid`, `status_p`, `first`, `last`, `pronouns`, `tpe`, `tpebank`,
          `birthplace`, `nationality`, `height`, `weight`, `hair_color`, `hair_length`,
          `skintone`, 
          `render`, `left foot`, `right foot`, `position`, `traits`, `pos_gk`, `pos_ld`, `pos_cd`,
          `pos_rd`, `pos_lwb`, `pos_cdm`, `pos_rwb`, `pos_lm`, `pos_cm`, `pos_rm`,
          `pos_lam`, `pos_cam`, `pos_ram`, `pos_st`, `acceleration`, `agility`, 
          `balance`, `jumping reach`, `natural fitness`, `pace`, `stamina`, `strength`, 
          `corners`, `crossing`, `dribbling`, `finishing`, `first touch`, `free kick`, 
          `heading`, `long shots`, `long throws`, `marking`, `passing`, `penalty taking`, 
          `tackling`, `technique`, `aggression`, `anticipation`, `bravery`, `composure`, 
          `concentration`, `decisions`, `determination`, `flair`, `leadership`, 
          `off the ball`, `positioning`, `teamwork`, `vision`, `work rate`, `aerial reach`, 
          `command of area`, `communication`, `eccentricity`, `handling`, `kicking`, 
          `one on ones`, `tendency to punch`, `reflexes`, `tendency to rush`, `throwing`)
        VALUES ({uid}, {status_p}, {first}, {last}, {pronouns}, {tpe}, {tpebank}, {birthplace},
          {nationality}, {height}, {weight}, {hair_color}, {hair_length}, {skintone}, 
          {render},
          {left}, {right}, {position}, {traits}, {pos_gk}, {pos_ld}, {pos_cd}, {pos_rd},
          {pos_lwb}, {pos_cdm}, {pos_rwb}, {pos_lm}, {pos_cm}, {pos_rm}, {pos_lam},
          {pos_cam}, {pos_ram}, {pos_st}, {acceleration}, {agility}, {balance}, 
          {jumpingreach}, {naturalfitness}, {pace}, {stamina}, {strength}, {corners}, 
          {crossing}, {dribbling}, {finishing}, {firsttouch}, {freekick}, {heading}, 
          {longshots}, {longthrows}, {marking}, {passing}, {penaltytaking}, {tackling}, 
          {technique}, {aggression}, {anticipation}, {bravery}, {composure}, {concentration}, 
          {decisions}, {determination}, {flair}, {leadership}, {offtheball}, {positioning}, 
          {teamwork}, {vision}, {workrate}, {aerialreach}, {commandofarea}, 
          {communication}, {eccentricity}, {handling}, {kicking}, {oneonones}, 
          {tendencytopunch}, {reflexes}, {tendencytorush}, {throwing});",
      uid = summary$uid,
      status_p = summary$status_p,
      first = summary$first,
      last = summary$last,
      pronouns = summary$pronouns,
      tpe = summary$tpe,
      tpebank = summary$tpebank,
      birthplace = summary$birthplace,
      nationality = summary$nationality,
      height = summary$height,
      weight = summary$weight,
      hair_color = summary$hair_color,
      hair_length = summary$hair_length,
      skintone = summary$skintone,
      render = summary$render,
      left = summary$`left foot`,
      right = summary$`right foot`,
      position = summary$position,
      traits = summary$traits,
      pos_gk  = summary$pos_gk,
      pos_ld  = summary$pos_ld,
      pos_cd  = summary$pos_cd,
      pos_rd  = summary$pos_rd,
      pos_lwb = summary$pos_lwb,
      pos_cdm = summary$pos_cdm,
      pos_rwb = summary$pos_rwb,
      pos_lm  = summary$pos_lm,
      pos_cm  = summary$pos_cm,
      pos_rm  = summary$pos_rm,
      pos_lam = summary$pos_lam,
      pos_cam = summary$pos_cam,
      pos_ram = summary$pos_ram,
      pos_st  = summary$pos_st,
      acceleration    = getSummaryValue(summary, "acceleration"),
      agility         = getSummaryValue(summary, "agility"),
      balance         = getSummaryValue(summary, "balance"),
      jumpingreach    = getSummaryValue(summary, "jumping reach"),
      naturalfitness  = getSummaryValue(summary, "natural fitness"),
      pace            = getSummaryValue(summary, "pace"),
      stamina         = getSummaryValue(summary, "stamina"),
      strength        = getSummaryValue(summary, "strength"),
      corners         = getSummaryValue(summary, "corners"),
      crossing        = getSummaryValue(summary, "crossing"),
      dribbling       = getSummaryValue(summary, "dribbling"),
      finishing       = getSummaryValue(summary, "finishing"),
      firsttouch      = getSummaryValue(summary, "first touch"),
      freekick        = getSummaryValue(summary, "free kick"),
      heading         = getSummaryValue(summary, "heading"),
      longshots       = getSummaryValue(summary, "long shots"),
      longthrows      = getSummaryValue(summary, "long throws"),
      marking         = getSummaryValue(summary, "marking"),
      passing         = getSummaryValue(summary, "passing"),
      penaltytaking   = getSummaryValue(summary, "penalty taking"),
      tackling        = getSummaryValue(summary, "tackling"),
      technique       = getSummaryValue(summary, "technique"),
      aggression      = getSummaryValue(summary, "aggression"),
      anticipation    = getSummaryValue(summary, "anticipation"),
      bravery         = getSummaryValue(summary, "bravery"),
      composure       = getSummaryValue(summary, "composure"),
      concentration   = getSummaryValue(summary, "concentration"),
      decisions       = getSummaryValue(summary, "decisions"),
      determination   = getSummaryValue(summary, "determination"),
      flair           = getSummaryValue(summary, "flair"),
      leadership      = getSummaryValue(summary, "leadership"),
      offtheball      = getSummaryValue(summary, "off the ball"),
      positioning     = getSummaryValue(summary, "positioning"),
      teamwork        = getSummaryValue(summary, "teamwork"),
      vision          = getSummaryValue(summary, "vision"),
      workrate        = getSummaryValue(summary, "work rate"),
      aerialreach     = getSummaryValue(summary, "aerial reach"),
      commandofarea   = getSummaryValue(summary, "command of area"),
      communication   = getSummaryValue(summary, "communication"),
      eccentricity    = getSummaryValue(summary, "eccentricity"),
      handling        = getSummaryValue(summary, "handling"),
      kicking         = getSummaryValue(summary, "kicking"),
      oneonones       = getSummaryValue(summary, "one on ones"),
      tendencytopunch = getSummaryValue(summary, "tendency to punch"),
      reflexes        = getSummaryValue(summary, "reflexes"),
      tendencytorush  = getSummaryValue(summary, "tendency to rush"),
      throwing        = getSummaryValue(summary, "throwing"),
      type = "set"
    )

    sendNewCreate(data = summary, username = userinfo$username)
    
    portalQuery(query = "COMMIT;", type = "set")
  }, error = function(e) {
    # If any error occurs, rollback the transaction and show an error message.
    portalQuery(query = "ROLLBACK;", type = "set")
    
    message("Error executing query: ", e)
    
    stop()
  })
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

#' @export
navigationCheck <- function(auth) {
  if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())){
    showToast(
      .options = constant$sslToastOptions,
      "error",
      "You do not have access to this page, please log in!"
    )
    
    FALSE
  } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)){
    showToast(
      .options = constant$sslToastOptions,
      "error",
      "You do not have access to this page."
    )
    
    FALSE
  } else if (!hasActivePlayer(auth$uid)) {
    showToast(
      .options = constant$sslToastOptions,
      "error",
      "You do not have an active player, please create one first!"
    )
    
    FALSE
  } else {
    TRUE
  }
}
