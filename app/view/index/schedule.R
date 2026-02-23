box::use(
  bslib,
  dplyr,
  reactable[colDef, reactable],
  rlang[is_empty],
  shiny,
  shiny.router[route_link],
  stringr[str_trim],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getSchedule],
  app / logic / ui / selector[leagueSelectInput],
  app / logic / ui / spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$uiOutput(ns("leagueSelector")) |>
          withSpinnerCustom(height = 20)
      ),
      bslib$card_body(
        shiny$h1("Schedule"),
        shiny$uiOutput(ns("schedule")) |>
          withSpinnerCustom(height = 80)
      )
    )
  )
}

#' @export
server <- function(id, updated, season) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      schedule <- shiny$reactive({
        shiny$req(input$selectedLeague)
        season <- season()
        league <- input$selectedLeague

        getSchedule(season = season, league = league)
      }) |> 
        shiny$bindCache(
          id,
          season(),
          input$selectedLeague
        ) |> 
        shiny$bindEvent(
          season(),
          input$selectedLeague
        )


      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = season(), session = session)
      })

      output$schedule <- shiny$renderUI({
        season <- season()
        league <- input$selectedLeague

        data <- schedule()
        
        if (season < 24) {
          data <- data |> 
            dplyr$select(!Division)
        }
          
        if (data |> is_empty()) {
          NULL
        } else {
          data |>
            dplyr$mutate(
              HomeScore = if (!"HomeScore" %in% names(data)) NA_character_ else HomeScore,
              AwayScore = if (!"AwayScore" %in% names(data)) NA_character_ else AwayScore,
              Penalties = if (!"Penalties" %in% names(data)) NA_character_ else Penalties,
              ExtraTime = if (!"ExtraTime" %in% names(data)) NA_character_ else ExtraTime
            ) |>
            dplyr$rename(
              Date = IRLDate
            ) |>
            dplyr$mutate(
              dplyr$across(
                c(HomeScore, AwayScore),
                function(x) ifelse(is.na(x), " ", x)
              ),
              Result = dplyr$case_when(
                Penalties == 1 & HomeScore > AwayScore ~ sprintf("p%s - %s", HomeScore, AwayScore),
                Penalties == 1 & HomeScore < AwayScore ~ sprintf("%s - %sp", HomeScore, AwayScore),
                ExtraTime == 1 & HomeScore > AwayScore ~ sprintf("e%s - %s", HomeScore, AwayScore),
                ExtraTime == 1 & HomeScore < AwayScore ~ sprintf("%s - %se", HomeScore, AwayScore),
                TRUE ~ sprintf("%s - %s", HomeScore, AwayScore)
              )
            ) |>
            dplyr$select(!c(HomeScore, AwayScore, ExtraTime, Penalties)) |>
            reactable(
              pagination = FALSE,
              searchable = TRUE,
              columns =
                list(
                  Date = colDef(width = 100),
                  Matchtype = colDef(width = 100),
                  Matchday = colDef(width = 150),
                  Home =
                  colDef(
                    cell = function(value) {
                      image <- shiny$img(
                        src = sprintf("static/logo/%s (Custom).png", value),
                        style = "height: 30px;",
                        title = value
                      )

                      shiny$tagList(
                        shiny$div(style = "display: inline-block; width: 30px;", image),
                        shiny$div(style = "font-size: 1.2rem", value)
                      )
                    }
                  ),
                  Away =
                  colDef(
                    cell = function(value) {
                      image <- shiny$img(
                        src = sprintf("static/logo/%s (Custom).png", value),
                        style = "height: 30px;",
                        title = value
                      )

                      shiny$tagList(
                        shiny$div(style = "display: inline-block; width: 30px;", image),
                        shiny$div(style = "font-size: 1.2rem", value)
                      )
                    }
                  ),
                  Result = 
                    colDef(
                      cell = function(value, index) {
                        if ((value |> str_trim()) == "-") {
                          value
                        } else {
                          shiny$a(
                            href = route_link(paste0("tracker/game?gid=", data$gid[index])),
                            value
                          )  
                        }
                      }
                    ),
                  gid = colDef(show = FALSE)
                )
            )
        }
      })
    }
  )
}
