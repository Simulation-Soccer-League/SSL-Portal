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
        shiny$h4("Player Footedness")
      ),
      bslib$card_body(
        shiny$tagList(
          shiny$p(
            "The footedness of a player is split into a main (20) and weak (10)
            foot based on the preferred footedness of the player selected at creation. 
            You can increase your weak foot proficiency by increments of 5 for 
            $7.5 million."
          ),
          bslib$layout_column_wrap(
            width = 1 / 2,
            shiny$selectInput(
              ns("left"),
              label = "Left Foot", 
              choices = NULL
            ), 
            shiny$selectInput(
              ns("right"),
              label = "Right Foot", 
              choices = NULL
            )
          )
        )
      )
    )
  )
}


#' @export
server <- function(id, cost, playerData) {
  shiny$moduleServer(id, function(input, output, session) {
    
    #### Reactives ####
    
    #### Observers ####
    ## Updates the selector based on current footedness
    shiny$observe({
      shiny$updateSelectInput(
        inputId = "left",
        choices = 
          seq(
            playerData()$`left foot`,
            20,
            by = 5
          )
      )
      
      shiny$updateSelectInput(
        inputId = "right",
        choices = 
          seq(
            playerData()$`right foot`,
            20,
            by = 5
          )
      )
    })
    
    ## Disables clashing traits
    
    ## Calculates sum of trait purchase
    shiny$observe({
      left <- input$left |> as.numeric()
      right <- input$right |> as.numeric()
      
      cost(
        ((left - playerData()$`left foot`) + (right - playerData()$`right foot`)) / 5 * 7500000
      )
    }) |> 
      shiny$bindEvent(
        input$left, input$right,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )
    
    return(input)
  })
}
