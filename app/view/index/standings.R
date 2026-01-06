box::use(
  bslib,
  dplyr,
  reactable[colDef, reactable],
  rlang[is_empty],
  shiny,
  shiny.router[route_link],
  tippy[tippy],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getStandings],
  app / logic / ui / reactableHelper[linkOrganization],
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
                seq_len(constant$currentSeason$season) |>
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
server <- function(id, updated) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      standings <- shiny$reactive({
        shiny$req(input$selectedLeague)
        season <- input$selectedSeason
        league <- input$selectedLeague

        getStandings(season = season, league = league)
      }) |> 
        shiny$bindCache(
          id,
          input$selectedSeason,
          input$selectedLeague
        ) |> 
        shiny$bindEvent(
          input$selectedSeason,
          input$selectedLeague,
          updated()
        )


      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = input$selectedSeason, session = session)
      }) 

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

        data <- standings() |> 
          dplyr$arrange(matchday, dplyr$desc(p), dplyr$desc(gd), dplyr$desc(gf))

        if (data |> is_empty()) {
          NULL
        } else if (league == 5){
          data |> 
            dplyr$select(!c(matchtype, season)) |> 
            dplyr$filter(nchar(matchday) == 1) |> 
            reactable(
              sortable = FALSE,
              pagination = FALSE,
              defaultExpanded = TRUE,
              groupBy = "matchday",
              defaultColDef = colDef(
                minWidth = 60,
                align = "center",
                style = function(value, index) {
                  list(
                    background =
                      dplyr$if_else(index %in% c(1,2, 5,6, 9, 10, 13, 14), constant$green, NA),
                    borderTop =
                      dplyr$if_else(index %in% seq(3, 15, by = 4), "solid", "none")
                  )
                }
              ),
              columns = list(
                matchday = colDef(name = ""),
                team = colDef(
                  name = "", 
                  width = 200, 
                  align = "left", 
                  cell = function(value) {
                    linkOrganization(value)
                  }
                ),
                mp = colDef(header = tippy("GP", "Games played", theme = "ssl")),
                w = colDef(header = tippy("W", "Wins", theme = "ssl")),
                d = colDef(header = tippy("D", "Draws", theme = "ssl")),
                l = colDef(header = tippy("L", "Losses", theme = "ssl")),
                gf = colDef(header = tippy("GF", "Goals scored", theme = "ssl")),
                ga = colDef(header = tippy("GA", "Goals conceded", theme = "ssl")),
                gd = colDef(header = tippy("GD", "Goal difference", theme = "ssl")),
                p = colDef(header = tippy("P", "Points", theme = "ssl"))
              )
            )
        } else {
          data |>
            dplyr$select(!c(matchtype, matchday, season)) |> 
            reactable(
              pagination = FALSE,
              sortable = FALSE,
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
                team = colDef(
                  name = "", 
                  width = 200, 
                  align = "left", 
                  cell = function(value) {
                    linkOrganization(value)
                  }
                ),
                mp = colDef(header = tippy("GP", "Games played", theme = "ssl")),
                w = colDef(header = tippy("W", "Wins", theme = "ssl")),
                d = colDef(header = tippy("D", "Draws", theme = "ssl")),
                l = colDef(header = tippy("L", "Losses", theme = "ssl")),
                gf = colDef(header = tippy("GF", "Goals scored", theme = "ssl")),
                ga = colDef(header = tippy("GA", "Goals conceded", theme = "ssl")),
                gd = colDef(header = tippy("GD", "Goal difference", theme = "ssl")),
                p = colDef(header = tippy("P", "Points", theme = "ssl"))
              )
            )
        }
      }) 
    }
  )
}
