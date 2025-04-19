box::use(
  bslib,
  dplyr,
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  shiny,
  stringr[str_to_upper],
  tippy[tippy],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getDraftClass],
  app / logic / ui / reactableHelper[draftClassReactable],
  app / logic / ui / selector[leagueSelectInput],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexCol, flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          colwidths = c(2, 10)
        ),
        shiny$selectInput(
          inputId = ns("selectedClass"),
          label = "Select a class",
          choices =
            c(
              1:(constant$currentSeason$season + 1) |>
                sort(decreasing = TRUE)
            )
        ),
        ""
      ),
      bslib$card_body(
        shiny$h1("Draft Class Tracker"),
        reactableOutput(ns("tracker")) |>
          withSpinnerCustom(height = 400)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Data
    draftclass <- shiny$reactive({
      getDraftClass(input$selectedClass)
    }) |>
      shiny$bindEvent(input$selectedClass)

    ### Output
    output$tracker <- renderReactable({
      data <- draftclass()

      data |>
        draftClassReactable()
    }) |>
      shiny$bindCache(input$selectedClass)
  })
}
