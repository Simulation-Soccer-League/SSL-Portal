box::use(
  bslib,
  dplyr,
  glue,
  shiny,
  shiny.router[route_link],
  stringr[str_to_lower, str_to_title, str_trim, str_replace_all],
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)

# Competition keys (CSS + Logo)
getCompetitionKeys <- function(matchType, matchDay, division) {

  # Shield comes from matchday
  if (matchDay == "Shield") {
    return("shield")
  }

  key <- str_to_lower(matchType)

  # Only customize when division exists
  if (!is.na(division)) {
    key<- paste(key, paste0("div", division))
  }

  key
}

# Result Card
#' @export
resultCard <- function(data, i, width = 40) {

  matchType <- data[i, "Matchtype"]
  matchDay  <- data[i, "Matchday"]
  division  <- data[i, "Division"]
  homeTeam  <- data[i, "Home"]
  awayTeam  <- data[i, "Away"]
  irlDate   <- data[i, "IRLDate"]
  homeScore <- data[i, "HomeScore"]
  awayScore <- data[i, "AwayScore"]
  gid       <- data[i, "gid"]
  penalties <- data[i, "Penalties"]
  extraTime <- data[i, "ExtraTime"]

  key <- getCompetitionKeys(matchType, matchDay, division)

  competitionLogo <- paste0(
    "/static/competition/",
    str_replace_all(key, " ", "_"),
    ".png"
  )

  stageLabel <- if (!is.na(matchDay)) matchDay else ""
  hasScore <- !is.na(homeScore) & !is.na(awayScore)
  scoreText <- if (hasScore) {
    dplyr$case_when(
      penalties == 1 & homeScore > awayScore ~ sprintf("p%s - %s", homeScore, awayScore),
      penalties == 1 & homeScore < awayScore ~ sprintf("%s - %sp", homeScore, awayScore),
      extraTime == 1 & homeScore > awayScore ~ sprintf("e%s - %s", homeScore, awayScore),
      extraTime == 1 & homeScore < awayScore ~ sprintf("%s - %se", homeScore, awayScore),
      TRUE ~ sprintf("%s - %s", homeScore, awayScore)
    )
  } else {
    "-"
  }

  card <- bslib$card(

    shiny$div(
      class = "result-card-inner",

      bslib$card_header(
        shiny$div(
          shiny$div(
            style = glue$glue(
              "display: inline-block; width: {width}px;",
              width = width
            ),
            linkOrganization(homeTeam, onlyImg = TRUE, height = width)
          ),
          shiny$strong(" - "),
          shiny$div(
            style = glue$glue(
              "display: inline-block; width: {width}px;",
              width = width
            ),
            linkOrganization(awayTeam, onlyImg = TRUE, height = width)
          ),
          align = "center"
        )
      ),

      bslib$card_body(
        shiny$h4(
          if ((scoreText |> str_trim()) == "-") {
            scoreText
          } else {
            shiny$a(
              href = route_link(
                paste0("tracker/game?gid=", gid)
                ),
              scoreText
            )  
          },
          class = paste("score", if (hasScore) "" else "is-empty")
        )
      ),

      bslib$card_footer(
        shiny$div(
          align = "center",
          shiny$div(stageLabel),
          shiny$div(irlDate)
        )
      )
    ),

    shiny$div(
      class = "competition-overlay",
      shiny$img(
        src = competitionLogo,
        alt = paste("Competition:", str_to_title(key))
      )
    )
  )

  shiny$tagAppendAttributes(
    card,
    class = "result-card",
    `data-league` = str_replace_all(key, " ", "-")
  )
}

#' @export
bslibCardContainer <- function(card) {
  shiny$div(
    style = 
      "display: flex;
            justify-content: center;
            max-width: 1000px;
            margin: auto;
          ",
    card
  )
}

#' @export
knockoutCard <- function(gameInfo) {
  
  hasScore <- !is.na(gameInfo$HomeScore) & !is.na(gameInfo$AwayScore)
  scoreText <- if (hasScore) {
    dplyr$case_when(
      gameInfo$Penalties == 1 & gameInfo$HomeScore > gameInfo$AwayScore ~ sprintf("p%s - %s", gameInfo$HomeScore, gameInfo$AwayScore),
      gameInfo$Penalties == 1 & gameInfo$HomeScore < gameInfo$AwayScore ~ sprintf("%s - %sp", gameInfo$HomeScore, gameInfo$AwayScore),
      gameInfo$ExtraTime == 1 & gameInfo$HomeScore > gameInfo$AwayScore ~ sprintf("e%s - %s", gameInfo$HomeScore, gameInfo$AwayScore),
      gameInfo$ExtraTime == 1 & gameInfo$HomeScore < gameInfo$AwayScore ~ sprintf("%s - %se", gameInfo$HomeScore, gameInfo$AwayScore),
      TRUE ~ sprintf("%s - %s", gameInfo$HomeScore, gameInfo$AwayScore)
    )
  } else {
    "-"
  }
  
  shiny$div(
    class = "knockout-card",
    shiny$div(linkOrganization(gameInfo$Home, onlyImg = TRUE, height = 40)), 
    shiny$div( 
      style = "font-size: 22px; font-weight: 700;", 
      if ((scoreText |> str_trim()) == "-") {
        scoreText
      } else {
        shiny$a(
          href = route_link(
            paste0("tracker/game?gid=", gameInfo$gid)
          ),
          scoreText
        )  
      } 
    ), 
    shiny$div(linkOrganization(gameInfo$Away, onlyImg = TRUE, height = 40)) 
  )
}
