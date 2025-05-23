box::use(
  bslib,
  dplyr,
  reactable[reactableOutput, renderReactable],
  shiny,
  stringr[str_to_title],
)

box::use(
  app / logic / db / get[getLeagueIndex],
  app / logic / ui / reactableHelper[recordReactable],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexCol, flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 4fr"),
          shiny$selectInput(
            inputId = ns("selectedLeague"),
            label = "League",
            choices =
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup",
                "WSFC"
              )
          )
        )
      ),
      bslib$card_body(
        shiny$tabsetPanel(
          header =
            shiny$tags$head(
              shiny$tags$style(
                shiny$HTML(
                  ".info-box {min-height: 65px;}
                    .info-box-icon {background: transparent; height: 65px; line-height: 65px;}
                    .info-box-content {padding-top: 0px; padding-bottom: 0px;}"
                )
              ),
              ## Imports all 6.0.0 Font Awesome Icons
              shiny$tags$style(
                "@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"
              )
            ),
          shiny$tabPanel(
            title = "Outfield Records",
            bslib$layout_columns(
              col_widths = c(4, 8),
              shiny$tagList(
                shiny$h3("Record for"),
                shiny$uiOutput(ns("recordList")) |>
                  withSpinnerCustom(height = 40)
              ),
              shiny$tagList(
                shiny$h3("Top 20", align = "center"),
                reactableOutput(
                  outputId = ns("leagueRecord")
                ) |>
                  withSpinnerCustom(height = 40)
              )
            )
          ),
          shiny$tabPanel(
            title = "Keeper Records",
            bslib$layout_columns(
              col_widths = c(4, 8),
              shiny$tagList(
                shiny$h3("Record for"),
                shiny$uiOutput(ns("recordListKeeper")) |>
                  withSpinnerCustom(height = 40)
              ),
              shiny$tagList(
                shiny$h3("Top 20", align = "center"),
                reactableOutput(
                  outputId = ns("leagueRecordKeeper")
                ) |>
                  withSpinnerCustom(height = 40)
              )
            )
          )
        )
      )
    ) # close card
  ) # close tagList
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      outfieldData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        league <- input$selectedLeague

        getLeagueIndex(league = league, season = "ALL")
      }) |>
        shiny$bindCache(input$selectedLeague)

      keeperData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        league <- input$selectedLeague

        getLeagueIndex(league = league, season = "ALL", outfield = FALSE)
      }) |>
        shiny$bindCache(input$selectedLeague)

      currentStatistic <- shiny$reactiveVal("goals")
      currentStatisticKeeper <- shiny$reactiveVal("won")

      #### UI OUTPUT ####
      outstatistics <- c(
        "goals", "assists", "xg",
        "distance run (km)", "key passes",
        "chances created", "tackles won",
        "interceptions", "key headers",
        "yellow cards", "red cards"
      )

      keepstatistics <- c("won", "clean sheets", "conceded", "save%")

      output$recordList <- shiny$renderUI({
        lapply(outstatistics, function(stat) {
          shiny$actionLink(
            session$ns(paste0(stat, "_record_click")),
            shiny$uiOutput(session$ns(paste0(stat, "_record")))
          )
        })
      })

      output$recordListKeeper <- shiny$renderUI({
        lapply(keepstatistics, function(stat) {
          shiny$actionLink(
            session$ns(paste0(stat, "_record_click")),
            shiny$uiOutput(session$ns(paste0(stat, "_record")))
          )
        })
      })

      ## Leader statistics
      leaderButton <- function(data, stat, selected) {
        leader <-
          data |>
          dplyr$select(
            name, club, dplyr$all_of(stat)
          ) |>
          dplyr$filter(
            dplyr$if_all(
              dplyr$all_of(stat),
              ~ .x == max(.x)
            )
          ) |>
          dplyr$slice_head(n = 1)

        selectedStat <- selected
        # Remove parenthesis from distance stat to make its label shorter
        trimmedStatTitle <- gsub("\\s\\(.+\\)", "", stat)

        shiny$tags$button(
          flexRow(
            shiny$tagList(
              flexCol(
                shiny$tagList(
                  shiny$tags$b(
                    paste(trimmedStatTitle |>
                            str_to_title()),
                    style = "font-size: 16px; text-transform: uppercase;"
                  ),
                  shiny$tags$b(leader[, stat] |>
                                 round(2))
                )
              ),
              flexRow(
                shiny$tagList(
                  shiny$icon("crown", style = "color: #BD9523;"),
                  shiny$tags$span(leader$name, style = "font-size: 16px;")
                ),
                style = "align-items: center; justify-content: flex-start; gap: 4px;"
              )
            ),
            style = "align-items: center; justify-content: space-between;"
          ),
          class = "career-record-button",
          style = dplyr$if_else(
            selectedStat == stat,
            "background-image: linear-gradient(to right, #4b8dad, #e5e5e5 40%);",
            ""
          )
        )
      }

      lapply(outstatistics, function(stat) {
        output[[paste0(stat, "_record")]] <- shiny$renderUI({
          outfieldData() |>
            leaderButton(stat = stat, selected = currentStatistic())
        })
      })

      lapply(keepstatistics, function(stat) {
        output[[paste0(stat, "_record")]] <- shiny$renderUI({
          keeperData() |>
            leaderButton(stat = stat, selected = currentStatisticKeeper())
        })
      })

      ## Observers to change the selected statistic for the top 20
      lapply(outstatistics, function(stat) {
        shiny$observe({
          currentStatistic(stat)
        }) |>
          shiny$bindEvent(
            input[[paste0(stat, "_record_click")]]
          )
      })

      lapply(keepstatistics, function(stat) {
        shiny$observe(
          currentStatisticKeeper(stat)
        ) |>
          shiny$bindEvent(
            input[[paste0(stat, "_record_click")]]
          )
      })

      #### REACTABLE OUTPUT ####
      top20Reactable <- function(data, stat) {
        data |>
          dplyr$select(
            name, club, apps, dplyr$all_of(stat)
          ) |>
          dplyr$arrange(
            dplyr$across(
              dplyr$starts_with(stat),
              dplyr$desc
            )
          ) |>
          dplyr$slice_head(n = 20) |>
          dplyr$mutate(
            RANK = seq_len(dplyr$n())
          ) |>
          dplyr$relocate(RANK) |>
          recordReactable()
      }

      output$leagueRecord <- renderReactable({
        outfieldData() |>
          top20Reactable(stat = currentStatistic())
      }) |>
        shiny$bindCache(input$selectedLeague, currentStatistic())

      output$leagueRecordKeeper <- renderReactable({
        keeperData() |>
          top20Reactable(stat = currentStatisticKeeper())
      }) |>
        shiny$bindCache(input$selectedLeague, currentStatisticKeeper())
    }
  )
}
