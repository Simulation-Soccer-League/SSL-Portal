box::use(
  bslib,
  dplyr,
  reactable[reactableOutput, renderReactable],
  shiny,
)

box::use(
  app / logic / constant,
  app / logic / db / get[getAcademyIndex],
  app / logic / ui / reactableHelper[indexReactable],
  app / logic / ui / spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(),
      bslib$card_body(
        shiny$tabsetPanel(
          header = shiny$h1("Outfield"),
          shiny$tabPanel(
            "Statistics",
            reactableOutput(ns("outfieldBasic")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Adv. Statistics",
            reactableOutput(ns("outfieldAdvanced")) |>
              withSpinnerCustom(height = 80)
          # ),
          # shiny$tabPanel(
          #   "Leaders",
          #   shiny$uiOutput(ns("outfieldLeaders")) |>
          #     withSpinnerCustom(height = 80)
          )
        ),
        shiny$tabsetPanel(
          header = shiny$h1("Keeper"),
          shiny$tabPanel(
            "Statistics",
            reactableOutput(ns("keeperBasic")) |>
              withSpinnerCustom(height = 80)
          ),
          shiny$tabPanel(
            "Adv. Statistics",
            reactableOutput(ns("keeperAdvanced")) |>
              withSpinnerCustom(height = 80)
          # ),
          # shiny$tabPanel(
          #   "Leaders",
          #   shiny$uiOutput(ns("keeperLeaders")) |>
          #     withSpinnerCustom(height = 80)
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, season) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      outfieldData <- shiny$reactive({
        season <- season()

        getAcademyIndex(season = season)
      }) |>
        shiny$bindCache(id, "outfield", season()) |> 
        shiny$bindEvent(season())

      keeperData <- shiny$reactive({
        season <- season()

        getAcademyIndex(season = season, outfield = FALSE)
      }) |>
        shiny$bindCache(id, "keeper", season()) |> 
        shiny$bindEvent(season())

      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        currentData <-
          outfieldData() |>
          dplyr$select(
            name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`, pid
          )

        currentData |>
          indexReactable()
      }) 

      output$outfieldAdvanced <- renderReactable({
        currentData <-
          outfieldData() |>
          dplyr$select(
            name:club,
            xg,
            xa:`fk shots`,
            `open play key passes`:`goals outside box`,
            `press%`:`pen adj xG`,
            pid
          )

        currentData |>
          indexReactable()
      })

      output$keeperBasic <- renderReactable({
        currentData <-
          keeperData() |>
          dplyr$select(
            name:`save%`, pid
          )

        currentData |>
          indexReactable()
      }) 

      output$keeperAdvanced <- renderReactable({
        currentData <-
          keeperData() |>
          dplyr$select(
            name:club,
            `penalties faced`:`xg prevented`, pid
          )

        currentData |>
          indexReactable()
      }) 
    }
  )
}
