box::use(
  shiny,
)


#' @export
leagueSelectInput <- function(season, session) {
  if (season != "ALL") {
    season <- season |> as.numeric()

    if (season < 5) {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices =
          c(
            "The League" = "Major League",
            "The Cup",
            "ALL"
          )
      )
    } else if (season %in% c(12, 18, 23)) {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices =
          c(
            "Major League",
            "Minor League",
            "The Cup",
            "WSFC",
            "ALL"
          )
      )
    } else if (season < 12) {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices =
          c(
            "Division 1" = "Major League",
            "Division 2" = "Minor League",
            "The Cup",
            "ALL"
          )
      )
    } else {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices =
          c(
            "Major League",
            "Minor League",
            "The Cup",
            "ALL"
          )
      )
    }
  } else {
    shiny$selectInput(
      inputId = session$ns("selectedLeague"),
      label = "League",
      choices =
        c(
          "Major / Division 1" = "Major League",
          "Minor / Division 2" = "Minor League",
          "The Cup",
          "WSFC",
          "ALL"
        )
    )
  }
}
