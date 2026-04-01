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
    leftChoices <- shiny$reactive({
      left <- playerData()$`left foot` |> as.numeric()
      right <- playerData()$`right foot` |> as.numeric()
      
      if (right == 20) {
        choices <- c(10, 15, 19)
      } else {
        choices <- 20
      }
      
      choices[choices >= left]
    })
    
    rightChoices <- shiny$reactive({
      left <- playerData()$`left foot` |> as.numeric()
      right <- playerData()$`right foot` |> as.numeric()
      
      if (left == 20) {
        choices <- c(10, 15, 19)
      } else {
        choices <- 20
      }
      
      choices[choices >= right]
    })
    
    #### Observers ####
    ## Updates the selector based on current footedness
    shiny$observe({
      print(leftChoices())
      print(rightChoices())
      
      shiny$updateSelectInput(
        inputId = "left",
        choices = leftChoices()
      )
      
      shiny$updateSelectInput(
        inputId = "right",
        choices = rightChoices()
      )
    })
    
    ## Disables clashing traits
    
    ## Calculates sum of trait purchase
    shiny$observe({
      left <- input$left |> as.numeric()
      right <- input$right |> as.numeric()
      
      changes <- 
        which(leftChoices() == left) - 
        which(leftChoices() == playerData()$`left foot`) + 
        which(rightChoices() == right) - 
        which(rightChoices() == playerData()$`right foot`)
      
      cost(7.5E6 * changes)
    }) |> 
      shiny$bindEvent(
        input$left, input$right,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )
    
    return(input)
  })
}
