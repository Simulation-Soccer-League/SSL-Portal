box::use(
  bslib,
  shiny
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)

# Competition keys (CSS + Logo)
getCompetitionKeys <- function(matchType, matchDay, division) {

  # Shield (comes from Matchday now)
  if (isTRUE(matchDay == "Shield")) {
    return(list(css = "shield", logo = "shield"))
  }

  # Major League
  if (isTRUE(matchType == "Major League")) {
    if (!is.na(division)) {
      return(list(css = "major", logo = paste0("major-div", division)))
    }
    return(list(css = "major", logo = "major"))
  }

  # Minor League
  if (isTRUE(matchType == "Minor League")) {
    if (!is.na(division)) {
      return(list(css = "minor", logo = paste0("minor-div", division)))
    }
    return(list(css = "minor", logo = "minor"))
  }

  # Cup
  if (isTRUE(matchType == "The Cup")) {
    return(list(css = "cup", logo = "cup"))
  }

  # WSFC
  if (isTRUE(matchType == "WSFC")) {
    return(list(css = "wsfc", logo = "wsfc"))
  }

  # Friendlies
  if (isTRUE(matchType == "Friendlies")) {
    return(list(css = "friendlies", logo = "friendlies"))
  }

  # fallback
  list(css = "friendlies", logo = "friendlies")
}




# Result Card
#' @export
resultCard <- function(data, i) {

  matchType <- data[i, "Matchtype"]
  matchDay <- data[i, "Matchday"]
  division  <- data[i, "Division"]
  homeTeam <- data[i, "Home"]
  awayTeam <- data[i, "Away"]
  irlDate  <- data[i, "IRLDate"]
  homeScore <- data[i, "HomeScore"]
  awayScore <- data[i, "AwayScore"]

  keys <- getCompetitionKeys(matchType, matchDay, division)

  cssLeagueKey <- keys$css
  logoKey <- keys$logo

  competitionLogo <- paste0(
    "/static/competition/",
    gsub("-", "_", logoKey),
    ".png"
  )

  stageLabel <- if (!is.na(matchDay)) matchDay else ""
  hasScore <- !is.na(homeScore) && !is.na(awayScore)
  scoreText <- if (hasScore) {
    paste(homeScore, awayScore, sep = " - ")
  } else {
    "-"
  }

  card <- bslib$card(

    shiny$div(
      class = "result-card-inner",

      bslib$card_header(
        shiny$div(
          shiny$div(
            style = "display: inline-block; width: 40px;",
            linkOrganization(homeTeam, onlyImg = TRUE, height = 40)
          ),
          shiny$strong(" - "),
          shiny$div(
            style = "display: inline-block; width: 40px;",
            linkOrganization(awayTeam, onlyImg = TRUE, height = 40)
          ),
          align = "center"
        )
      ),

      bslib$card_body(
        shiny$h4(
          scoreText,
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
        alt = paste("Competition:", logoKey)
      )
    )
  )

  shiny::tagAppendAttributes(
    card,
    class = "result-card",
    `data-league` = cssLeagueKey
  )
}
