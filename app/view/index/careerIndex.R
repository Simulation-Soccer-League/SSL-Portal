box::use(
  bslib,
  dplyr,
  purrr[map],
  reactable[reactableOutput, renderReactable],
  shiny,
  shinyjs,
)

box::use(
  app / logic / constant,
  app / logic / db / get[getLeagueIndex],
  app / logic / ui / reactableHelper[indexReactable, recordReactable],
  app / logic / ui / selector[leagueSelectInput],
  app / logic / ui / spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 1fr 2fr"),
          shiny$uiOutput(ns("leagueSelector")),
          shiny$uiOutput(ns("retiredSelector")),
          ""
        )
      ),
      bslib$card_body(
        shiny$h1("Outfield"),
        shiny$tabsetPanel(
          shiny$tabPanel(
            "Statistics",
            reactableOutput(ns("outfieldBasic")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Adv. Statistics",
            reactableOutput(ns("outfieldAdvanced")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Leaders",
            shiny$uiOutput(ns("outfieldLeaders")) |>
              withSpinnerCustom(height = 80)
          )
        ),
        shiny$h1("Keeper"),
        shiny$tabsetPanel(
          shiny$tabPanel(
            "Statistics",
            reactableOutput(ns("keeperBasic")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Adv. Statistics",
            reactableOutput(ns("keeperAdvanced")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Leaders",
            shiny$uiOutput(ns("keeperLeaders")) |>
              withSpinnerCustom(height = 80)
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      season <- "ALL"
      
      #### DATA GENERATION ####
      outfieldData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        league <- input$selectedLeague

        getLeagueIndex(season = season, league = league)
      }) |>
        shiny$bindCache(
          id,
          "outfield", 
          input$selectedLeague
        )

      keeperData <- shiny$reactive({
        shiny$req(input$selectedLeague)
        league <- input$selectedLeague

        getLeagueIndex(season = season, league = league, outfield = FALSE)
      }) |>
        shiny$bindCache(
          id,
          "keeper", 
          input$selectedLeague
        )
      #### UI OUTPUT ####
      output$leagueSelector <- shiny$renderUI({
        leagueSelectInput(season = season, session = session)
      })
      
      output$retiredSelector <- shiny$renderUI({
        shiny$checkboxInput(
          session$ns("retired"),
          label = "Remove retired players",
          value = FALSE
        )  
      })
      
      outstatistics <- c(
        "goals",
        "assists",
        "player of the match",
        "distance run (km)",
        "successful passes",
        "chances created",
        "tackles won",
        "interceptions",
        "yellow cards",
        "red cards"
      )

      output$outfieldLeaders <- shiny$renderUI({
        map(
          .x = outstatistics,
          .f = function(chosenStat) {
            shiny$tagList(
              shiny$div(
                class = "leader-table",
                style = "width: 80%",
                reactableOutput(session$ns(paste0(chosenStat, "_leader")))
              )
            )
          }
        ) |> 
          shiny$div(class = "leader-table-group")

      })

      lapply(outstatistics, function(stat) {
        output[[paste0(stat, "_leader")]] <- renderReactable({
          data <- outfieldData() |> 
            dplyr$filter(input$retired == FALSE | 
                           max_season == max(max_season, na.rm = FALSE))

          data |>
            dplyr$select(
              name, club, dplyr$all_of(stat),
              pid
            ) |>
            dplyr$arrange(
              dplyr$across(
                dplyr$starts_with(stat),
                dplyr$desc
              )
            ) |>
            dplyr$slice_head(n = 10) |>
            recordReactable()
        })
      })

      keepstatistics <- c("won", "clean sheets", "conceded", "save%")

      output$keeperLeaders <- shiny$renderUI({
        map(
          .x = keepstatistics,
          .f = function(chosenStat) {
            shiny$tagList(
              shiny$div(
                class = "leader-table",
                style = "width: 80%",
                reactableOutput(session$ns(paste0(chosenStat, "_leader")))
              )
            )
          }
        ) |> 
          shiny$div(class = "leader-table-group")

      })

      lapply(keepstatistics, function(stat) {
        output[[paste0(stat, "_leader")]] <- renderReactable({
          data <- keeperData() |> 
            dplyr$filter(input$retired == FALSE | 
                           max_season == max(max_season, na.rm = FALSE))

          data |>
            dplyr$select(
              name, club, dplyr$all_of(stat),
              pid
            ) |>
            dplyr$arrange(
              dplyr$across(
                dplyr$starts_with(stat),
                dplyr$desc
              )
            ) |>
            dplyr$slice_head(n = 10) |>
            recordReactable()
        })
      })

      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        data <- outfieldData() 
        
        currentData <-
          data |>
          dplyr$select(
            name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`,
            max_season,
            pid
          ) |> 
          dplyr$filter(input$retired == FALSE | 
                         max_season == max(max_season, na.rm = FALSE))
        
        currentData |>
          dplyr$select(!max_season) |> 
          indexReactable()
      }) |> 
        shiny$bindCache(
          id,
          "outfield", 
          input$selectedLeague,
          input$retired
        )
      
      output$outfieldAdvanced <- renderReactable({
        data <- outfieldData()

        currentData <-
          data |>
          dplyr$select(
            name:club,
            xg,
            xa:`fk shots`,
            `open play key passes`:`goals outside box`,
            `press%`:`pen adj xG`,
            max_season,
            pid
          ) |> 
          dplyr$filter(input$retired == FALSE | 
                         max_season == max(max_season, na.rm = FALSE))

        currentData |>
          dplyr$select(!max_season) |> 
          indexReactable()
      }) |> 
        shiny$bindCache(
          id,
          "outfieldAdv", 
          input$selectedLeague,
          input$retired
        )
      
      output$keeperBasic <- renderReactable({
        data <- keeperData()

        currentData <-
          data |>
          dplyr$select(
            name:`save%`,
            max_season,
            pid
          ) |> 
          dplyr$filter(input$retired == FALSE | 
                         max_season == max(max_season, na.rm = FALSE))

        currentData |>
          dplyr$select(!max_season) |> 
          indexReactable()
      }) |> 
        shiny$bindCache(
          id,
          "keeper", 
          input$selectedLeague,
          input$retired
        )
      
      output$keeperAdvanced <- renderReactable({
        data <- keeperData()

        currentData <-
          data |>
          dplyr$select(
            name:club,
            `penalties faced`:`xg prevented`,
            max_season,
            pid
          ) |> 
          dplyr$filter(input$retired == FALSE | 
                         max_season == max(max_season, na.rm = FALSE))

        currentData |>
          dplyr$select(!max_season) |> 
          indexReactable()
      }) |> 
        shiny$bindCache(
          id,
          "keeperAdv", 
          input$selectedLeague,
          input$retired
        )
    }
  )
}
