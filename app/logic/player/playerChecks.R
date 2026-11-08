box::use(
  assertthat[assert_that,],
  dplyr,
  htmltools,
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
    str_to_upper,
    str_trim
    ],
  tibble[tibble],
  tidyr[pivot_longer, pivot_wider, replace_na],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/discord[sendNewCreate],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/get/getPlayer[
    getActivePid, 
    getPlayer,
    hasPlayer,
  ],
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
eligibleRegress <- function(data){
  data$tpebank < 0
}

#' @export
#' type = "create" is for new created players and should not need a current build
validateBuild <- function(pid, input, type = "create", ...) {
  
  if (!(pid |> is.null())) {
    current <- getPlayer(pid)  
  } else {
    current <- tibble(
      tpe = constant$startingTPE,
      timesregressed = 0
    )
  }
  
  if (type == "redistribution") {
    assert_that(
      eligibleRedist(current), 
      msg = "You are not eligible for a redistribution."
    )
  }
  
  if (type == "reroll") {
    assert_that(
      eligibleReroll(current), 
      msg = "You are not eligible for a reroll."
    )
  }
  
  if (type == "create") {
    assert_that(
      hasPlayer(uid), 
      msg = "You already have a non-retiring/non-retired player in the league."
    )
  }
  
  # Calculates the total cost of the new build
  tpeUsed <- 0
  print("Attributes")
  for (att in (constant$attributes$attribute)) {
    newValue <- input[[att |> str_remove_all(" ")]]
    curValue <- current[[att |> str_to_lower()]]
    
    if (newValue |> length() == 1){
      assert_that((newValue %% 1) == 0, msg = "All attribute values must be a whole number")
      # assert_that(newValue >= 5, "All attribute values must be at minimum 5.")
      # assert_that(newValue <= 20, "All attribute values must be at maximum 20.")
      assert_that(
        newValue %in% constant$tpeCost$value, 
        msg = sprintf("The attribute value for %s is not correct.", att)
      )
      
      if (type == "update") {
        assert_that(newValue >= curValue, msg = "An update cannot regress attributes.")
      }
      
      tpeUsed <- tpeUsed + constant$tpeCost$cumCost[constant$tpeCost$value == newValue]
    }
  }
  print("Locked attributes")
  assert_that(
    input[["Stamina"]] == 20, 
    input[["NaturalFitness"]] == 20, 
    msg = "Stamina and Natural Fitness is locked to 20 and cannot be changed."
  )
  
  # Removes two 20 costs from the total spent from the two fixed attributes
  tpeUsed <- tpeUsed - 2*constant$tpeCost$cumCost[constant$tpeCost$value == 20]
  
  physicalMax <- 20 + max(current$timesregressed - 2, 0)
  print("Physicals")
  assert_that(
    input[["Acceleration"]] <= physicalMax,
    input[["Agility"]] <= physicalMax,
    input[["Balance"]] <= physicalMax,
    input[["JumpingReach"]] <= physicalMax,
    input[["Pace"]] <= physicalMax,
    input[["Strength"]] <= physicalMax,
    msg = sprintf("All physical attributes must be at most at your current max, %s.", physicalMax)
  )
  
  if (type == "regression") {
    assert_that((current$tpe - tpeUsed) <= 24, msg = "You cannot regress to more than 24 TPE in the bank.")  
  } 
  
  print(tpeUsed)
  assert_that((current$tpe - tpeUsed) >= 0, msg = "Your build exceeds the number of TPE you have earned.")
  
  if (type %in% c("redistribution", "reroll")) {
    assert_that(
      (input$footedness |> str_to_lower()) %in% c("left", "right"), 
      msg = "You cannot have footedness other than 'left' or 'right'."
    )  
  } 
  
  print("Only outfield specific things remaining")
  
  if (input$playerType |> is.null()){
    isOutfield <- (current$pos_gk != 20 & type == "update")
  } else {
    isOutfield <- (input$playerType |> str_to_lower()) == "outfield"
  }
  
  print(isOutfield)
  
  if (isOutfield) {
    positions <- constant$positions
    print("Position check")
    if (type != "update") {
      assert_that(
        all(input$primary %in% names(positions)),
        all(input$secondary %in% names(positions)),
        !any(input$primary %in% input$secondary),
        !any(input$secondary %in% input$primary),
        msg = "Positions must be uniquely defined and be one of the standard positions."
      )
      
      assert_that(
        all(
          input$traits |> str_to_lower() %in% 
            (constant$jsonTraits |> str_to_lower())
        ),
        msg = "Traits must be from the allowed list."
      )
    }
    
    if (type %in% c("redistribution", "reroll")) {
      curPos <- current |> 
        dplyr$select(dplyr$contains("pos_")) |> 
        pivot_longer(cols = dplyr$everything()) |> 
        dplyr$mutate(
          name = str_remove_all(name, "pos_") |> 
            str_to_upper()
        )
      
      print(curPos)
      
      posPrim <- 
        positions[
          names(positions) %in% 
            (curPos |> 
               dplyr$filter(value == 20) |> 
               dplyr$select(name) |> unlist()
            )
        ]
      
      posSec <- 
        positions[
          names(positions) %in% 
            (curPos |> 
               dplyr$filter(value < 20 & value > 5) |> 
               dplyr$select(name) |> unlist()
            )
        ]
      
      
      assert_that(
        (input$primary |> length()) == (posPrim |> length()), 
        msg = sprintf("You can only have the %s primary positions.", posPrim |> length())
      )
      assert_that(
        (input$secondary |> length()) == (posSec |> length()), 
        msg = sprintf("You can only have the %s secondary positions.", posSec |> length())
      )
      
    }
  
  }
  
}

#' @export
updateSummary <- function(current, input, type = "update"){
  summary <- tibble(tpe = current$tpe)
  
  tryCatch({
    
    validateBuild(current$pid, input, type)
    
    
    ## Adds player information, traits and positions
    if(type %in% c("redistribution", "reroll")){
      summary <-
        tibble(
          birthplace = input$birthplace |> htmltools$htmlEscape(),
          height = input$height,
          weight = input$weight,
          hair_color = input$hairColor,
          hair_length = input$hairLength,
          skintone = input$skintone,
          `left foot` = 
            dplyr$case_when(
              input$footedness == "Right" & current$`right foot` == 20 ~ 
                max(current$`left foot`, 10),
              input$footedness == "Right" & current$`right foot` != 20 ~
                max(current$`right foot`, 10),
              TRUE ~ 20
            ),
          `right foot` = 
            dplyr$case_when(
              input$footedness == "Left" & current$`left foot` == 20 ~ 
                max(current$`right foot`, 10),
              input$footedness == "Left" & current$`left foot` != 20 ~
                max(current$`left foot`, 10),
              TRUE ~ 20
            ),
          pronouns = 
            input$pronouns |>
            htmltools$htmlEscape() |> 
            sort() |> 
            paste0(collapse = constant$traitSep)
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
        
        summary$traits <- 
          paste0(
            input$traits |> 
              htmltools$htmlEscape(), 
            collapse = constant$traitSep
          )
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
      dplyr$mutate(
        dplyr$across(
          dplyr$where(is.numeric),
          ~ replace_na(.x, 5)
        ),
        dplyr$across(
          dplyr$where(is.character),
          ~ replace_na(.x, "")
        )
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
      # dplyr$mutate(
      #   dplyr$across(
      #     dplyr$everything(),
      #     ~ replace_na(.x, "5")
      #   )
      # ) |> 
      dplyr$filter(old != new) |> 
      dplyr$select(attribute = name, old, new)
  }, error = function(e) {
    stop(e$message)
  })
}

#' @export
hasActivePlayer <- function(uid){
  actives <- getActivePid(uid)
  
  !is_empty(actives)
}

#' @export
checkDuplicatedNames <- function(first, last){
  first <- str_trim(first)
  last <- str_trim(last)
  
  duplicates <- 
    portalQuery(
      query = "
        SELECT *
        FROM playerdata
        WHERE `first` = {first}
          AND `last`  = {last};
      ",
      first = first,
      last  = last
    ) 
  
  duplicates |> 
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
      render = input$render |> htmltools$htmlEscape(),
      `left foot` = dplyr$if_else(input$footedness == "Right", 10, 20),
      `right foot` = dplyr$if_else(input$footedness == "Right", 20, 10),
    )
  
  tryCatch({
    validateBuild(pid = NULL, input)
    
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
      
      summary$traits <- 
        paste0(
          input$traits |> 
            htmltools$htmlEscape(), 
          collapse = constant$traitSep
        )
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
  }, error = function(e){
    stop(e$message)
  })
  
}

#' @export
submitBuild <- function(input, bankedTPE, userinfo){
  
  tryCatch({
    # Begin the transaction
    portalQuery(
      query = "START TRANSACTION;",
      type = "set"
    )
    
    validateBuild(pid = NULL, input, uid = userinfo$uid)
    
    summary <- 
      tibble(
        uid = userinfo$uid,
        status_p = -1,
        first = str_trim(input$firstName) |> htmltools$htmlEscape(),
        last = str_trim(input$lastName) |> htmltools$htmlEscape(),
        tpe = constant$startingTPE,
        tpebank = bankedTPE(),
        birthplace = input$birthplace |> htmltools$htmlEscape(),
        nationality = input$nationality,
        height = input$height,
        weight = input$weight,
        hair_color = input$hairColor,
        hair_length = input$hairLength,
        skintone = input$skinColor,
        render = input$render |> htmltools$htmlEscape(),
        `left foot` = dplyr$if_else(input$footedness == "Right", 10, 20),
        `right foot` = dplyr$if_else(input$footedness == "Right", 20, 10),
        pronouns = 
          input$pronouns |>
          htmltools$htmlEscape() |> 
          sort() |> 
          paste0(collapse = constant$traitSep)
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
      
      assert_that(all(input$traits %in% (constant$traits |> unlist(use.names = FALSE))), msg = "The traits you have submitted does not match the available traits.")
      
      summary$traits <- 
        paste0(
          input$traits |> 
            htmltools$htmlEscape(), 
          collapse = constant$traitSep
        )
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
    with_tz("America/Los_Angeles") |>
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
    force_tz("America/Los_Angeles") |>
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
