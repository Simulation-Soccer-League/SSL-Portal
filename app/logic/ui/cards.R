box::use(
  bslib,
  shiny,
  stringr[str_replace_all]
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)

# Competition keys (CSS + Logo)
getCompetitionKeys <- function(matchType, matchDay, division) {

  # Shield comes from matchday
  if (matchDay == "Shield") {
    return(list(css = "shield", logo = "shield"))
  }

  key <- tolower(matchType)

  css <- key
  logo <- key

  # Only customize when division exists
  if (!is.na(division)) {
    key<- paste(key, paste0("div", division))
  }

  list(css = css, logo = logo)
}

# Result Card
#' @export
resultCard <- function(data, i) {

  matchType <- data[i, "Matchtype"]
  matchDay  <- data[i, "Matchday"]
  division  <- data[i, "Division"]
  homeTeam  <- data[i, "Home"]
  awayTeam  <- data[i, "Away"]
  irlDate   <- data[i, "IRLDate"]
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
        alt = paste("Competition:", str_to_title(key))
      )
    )
  )

  shiny$tagAppendAttributes(
    card,
    class = "result-card",
    `data-league` = cssLeagueKey
  )
}
