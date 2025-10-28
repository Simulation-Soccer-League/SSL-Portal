# modules/history.R
box::use(
  bslib,
  dplyr,
  shiny,
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  lubridate[as_datetime],
  rlang[is_empty]
)

box::use(
  app/logic/constant,
  app/logic/db/get[getActivePlayer, getTpeHistory, getUpdateHistory, getBankHistory],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$card(
    bslib$card_header(shiny$h3("Player History")),
    bslib$card_body(
      shiny$tabsetPanel(
        shiny$tabPanel("TPE History",    reactableOutput(ns("tpe"))),
        shiny$tabPanel("Update History", reactableOutput(ns("update"))),
        shiny$tabPanel("Bank History",   reactableOutput(ns("bank")))
      )
    )
  )
}

#' @export
server <- function(id, updated, playerData) {
  shiny$moduleServer(id, function(input, output, session) {
    historyTPE <- shiny$reactive({
      playerData()$pid |> 
        getTpeHistory()
    }) |> 
      shiny$bindCache(playerData()$pid, updated()) |> 
      shiny$bindEvent(updated())
    
    output$tpe <- renderReactable({
      data <- playerData()
      tpe <- historyTPE()
      
      if (tpe |> is_empty()) {
        NULL
      } else {
        tpe |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
      }
    }) |> 
      shiny$bindCache(playerData()$pid, updated())
    
    output$update <- renderReactable({
      data <- playerData()
      updates <- getUpdateHistory(data$pid)
      if (updates |> is_empty()) {
        NULL
      } else {
        updates |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
      }
    }) |> 
      shiny$bindCache(playerData()$pid, updated())
    
    output$bank <- renderReactable({
      data <- playerData()
      bank <- getBankHistory(data$pid)
      if (bank |> is_empty()) {
        NULL
      } else {
        bank |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE)),
                Transaction = colDef(
                  format = colFormat(
                    digits = 0,
                    separators = TRUE, 
                    currency = "USD"
                  )
                )
              )
          )
      }
    }) |> 
      shiny$bindCache(playerData()$pid, updated())
  })
}
