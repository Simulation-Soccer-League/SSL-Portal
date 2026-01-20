box::use(
  bslib,
  dplyr[if_else],
  shiny,
  stringr[str_replace_all],
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)
# Division and Matchday parsing
parseDivisionMatchday <- function(matchDay) {

  #New Format (1.2, 2.5)
  if (grepl("^\\d+\\.\\d+$", matchDay)) {
    parts <- strsplit(matchDay, "\\.")[[1]]
    return(list(
      division = parts[1],
      matchday = parts[2]
    ))
  }

  #Old Format
  list(
    division = NULL,
    matchday = matchDay
  )
}


# Competition key (used for colour + logo)

getCompetitionKey <- function(matchType, matchDay) {

  parsed <- parseDivisionMatchday(matchDay)

  if (matchType == 0 && matchDay == "Shi") {
    return("shield")
  }
  #Majors / Minors with division
  if (matchType == 1 && !is.null(parsed$division)) {
    return(paste0("major-div", parsed$division))
  }

  if (matchType == 2 && !is.null(parsed$division)) {
    return(paste0("minor-div", parsed$division))
  }
  #Majors / Minors without division and other competitions
  if (matchType == 1) return("major")
  if (matchType == 2) return("minor")
  if (matchType == 0) return("cup")
  if (matchType == 5) return("wsfc")

  "friendlies"
}


# Stage label (Footer text)
getStageLabel <- function(matchType, matchDay) {

  if (matchType %in% c(1, 2)) {

    parsed <- parseDivisionMatchday(matchDay)

    if (!is.null(parsed$division)) {
      return(
        paste("D", parsed$division, " MD", parsed$matchday)
      )
    }

    return(paste("MD", matchDay))
  }

  if (matchType == 0 && matchDay == "Shi") {
    return("Shield")
  }

  if (matchType == 0) {

    if (grepl("^[A-D][0-9]+$", matchDay)) {
      group <- substr(matchDay, 1, 1)
      round <- substr(matchDay, 2, nchar(matchDay))
      return(paste("Group", group, "MD", round))
    }

    if (grepl("^(FR|R16|QF|SF)[12]$", matchDay)) {

      stage <- sub("[12]$", "", matchDay)
      leg <- substr(matchDay, nchar(matchDay), nchar(matchDay))

      stageLabel <- switch(
        stage,
        "FR"  = "First Round",
        "R16" = "Round of 16",
        "QF"  = "Quarter Final",
        "SF"  = "Semi Final",
        stage
      )

      return(paste(stageLabel, "Leg", leg))
    }

    return(
      switch(
        as.character(matchDay),
        "Q"   = "Qualifying",
        "R16" = "Round of 16",
        "QF"  = "Quarter Final",
        "SF"  = "Semi Final",
        "F"   = "Final",
        matchDay
      )
    )
  }

  if (matchType == 5) {

    if (matchDay == "Pre") {
      return("Pre Season")
    }

    if (grepl("^[A-D][0-9]+$", matchDay)) {
      group <- substr(matchDay, 1, 1)
      round <- substr(matchDay, 2, nchar(matchDay))
      return(paste("Group", group, "MD", round))
    }

    return(
      switch(
        as.character(matchDay),
        "QF" = "Quarter Final",
        "SF" = "Semi Final",
        "F"  = "Final",
        matchDay
      )
    )
  }

  switch(
    as.character(matchDay),
    "Pre" = "Pre Season",
    "Friendly"
  )
}

#Result Card
#' @export
resultCard <- function(data, i) {

  competitionKey <- getCompetitionKey(
    data[i, "MatchType"],
    data[i, "MatchDay"]
  )

  competitionLogo <- paste0(
    "/static/competition/",
    gsub("-", "_", competitionKey),
    ".png"
  )

  stageLabel <- getStageLabel(
    data[i, "MatchType"],
    data[i, "MatchDay"]
  )

  homeScore <- data[i, "HomeScore"]
  awayScore <- data[i, "AwayScore"]

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
            linkOrganization(data[i, "Home"], onlyImg = TRUE, height = 40)
          ),
          shiny$strong(" - "),
          shiny$div(
            style = "display: inline-block; width: 40px;",
            linkOrganization(data[i, "Away"], onlyImg = TRUE, height = 40)
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
          shiny$div(data[i, "IRLDate"])
        )
      )
    ),

    shiny$div(
      class = "competition-overlay",
      shiny$img(
        src = competitionLogo,
        alt = paste("Competition:", competitionKey)
      )
    )
  )

  shiny::tagAppendAttributes(
    card,
    class = "result-card",
    `data-league` = competitionKey
  )
}
