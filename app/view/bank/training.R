box::use(
  bslib,
  shiny,
)

box::use(
  ,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$h4("Seasonal Training")
      ),
      bslib$card_body(
        shiny$tagList(
          shiny$p(
            "You may purchase at most 18 TPE from individual training per season. Each TPE purchased
            cost $425 000 for a total cost of $7.65 million for the entire 18 TPE."
          ),
          shiny$sliderInput(
            ns("individualTraining"),
            label = "Pull the slider to the amount of TPE you want to purchase:",
            min = 0,
            max = 18,
            value = 0,
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        )
      )
    )
  )
}


#' @export
server <- function(id, cost, playerData) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observe({
      shiny$updateSliderInput(
        inputId = "individualTraining",
        max = 18 - playerData()$purchasedTPE
      )
    })
    
    
    shiny$observe({
      cost(input$individualTraining * 425000)
    }) |> 
      shiny$bindEvent(
        input$individualTraining
      )
  })
}
