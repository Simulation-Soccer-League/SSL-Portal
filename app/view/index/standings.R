box::use(
  bslib,
  dplyr,
  reactable[colDef, reactable],
  rlang[is_empty],
  shiny,
  tippy[tippy],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getStandings],
  app / logic / ui / selector[leagueSelectInput],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 4fr 1fr"),
          shiny$selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices =
              c(
                1:constant$currentSeason$season |>
                  sort(decreasing = TRUE),
                "ALL"
              )
          ),
          "",
          shiny$uiOutput(ns("leagueSelector")) |>
            withSpinnerCustom(height = 20)
        )
      ),
      bslib$card_body(
        shiny$h1("Standings"),
        shiny$uiOutput(ns("standings")) |>
          withSpinnerCustom(height = 80)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      standings <- shiny$reactive({
        shiny$req(input$selectedLeague)
        season <- input$selectedSeason
        league <- input$selectedLeague

        getStandings(season = season, league = league)
      })


      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = input$selectedSeason, session = session)
      }) |>
        shiny$bindCache(input$selectedSeason)

      output$standings <- shiny$renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague

        if (season == "ALL") {
          relegation <- FALSE
        } else if (season |> as.numeric() < 5 | season |> as.numeric() > 11) {
          relegation <- FALSE
        } else {
          relegation <- TRUE
        }

        data <- standings()

        if (data |> is_empty()) {
          NULL
        } else {
          data |>
            dplyr$select(!GoalDifference) |>
            reactable(
              pagination = FALSE,
              defaultColDef = colDef(
                minWidth = 60,
                align = "center",
                style = function(value, index) {
                  list(
                    background =
                      dplyr$if_else(index > 6 & relegation & league == 1,
                        constant$red,
                        dplyr$if_else(index < 3 & relegation & league == 2,
                          constant$green,
                          NA
                        )
                      ),
                    borderTop =
                      dplyr$if_else(
                        (
                          (index == 7 & relegation & league == 1)
                          | (index == 3 & relegation & league == 2)
                        ),
                        "solid",
                        "none"
                      )
                  )
                }
              ),
              columns = list(
                Team = colDef(name = "", width = 200, align = "left", cell = function(value) {
                  image <- shiny$img(
                    src = sprintf("static/logo/%s (Custom).png", value),
                    style = "height: 30px;",
                    alt = value,
                    title = value
                  )

                  list <-
                    shiny$tagList(
                      flexRow(
                        style = "align-items: center; gap: 8px;",
                        shiny$tagList(
                          image,
                          shiny$span(class = "truncated-text", value)
                        )
                      )
                    )
                }),
                MatchesPlayed = colDef(header = tippy("GP", "Games played", theme = "ssl")),
                Wins = colDef(header = tippy("W", "Wins", theme = "ssl")),
                Draws = colDef(header = tippy("D", "Draws", theme = "ssl")),
                Losses = colDef(header = tippy("L", "Losses", theme = "ssl")),
                GoalsFor = colDef(header = tippy("GF", "Goals scored", theme = "ssl")),
                GoalsAgainst = colDef(header = tippy("GA", "Goals conceded", theme = "ssl")),
                Points = colDef(header = tippy("P", "Points", theme = "ssl"))
              )
            )
        }
      }) |>
        shiny$bindCache(input$selectedSeason, input$selectedLeague)
    }
  )
}
