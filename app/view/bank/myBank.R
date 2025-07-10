box::use(
  bslib,
  dplyr,
  lubridate[
    as_datetime, 
  ],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  scales[label_currency],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stringr[str_detect, str_remove, str_split, str_to_upper],
  tidyr[pivot_longer],
)

box::use(
  app/logic/constant,
  app/logic/db/get[
    getActivePlayer, 
    getPlayer, 
  ],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updateTPE],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
  app/logic/player/playerHistory,
  app/logic/player/playerInfo,
  app/logic/ui/spinner[withSpinnerCustom],
  app/view/bank/positions,
  app/view/bank/training,
  app/view/bank/traits,
  app/view/bank/footedness,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server <- function(id, auth, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else if (!hasActivePlayer(auth$uid)) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ANY ACTIVE PLAYER, PLEASE CREATE ONE FIRST."
      })
    } else {
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        shiny$tagList(
          shiny$verbatimTextOutput(ns("cost")),
          bslib$layout_column_wrap(
            width = 1 / 2,
            playerInfo$ui(ns("info")),
            playerHistory$ui(ns("history"))
          ),
          bslib$card(
            bslib$card_header(
              shiny$h3("Player Store Purchases")
            ),
            bslib$card_body(
              training$ui(ns("training")),
              traits$ui(ns("traits")),
              if (playerData()$pos_gk != 20) {
                positions$ui(ns("positions"))
              },
              footedness$ui(ns("footedness"))
            )
          )
        )
        
      })
      
      #### REACTIVES ####
      playerData <- shiny$reactive({
        getActivePlayer(auth$uid) |> 
          getPlayer()
      }) |> 
        shiny$bindEvent(updated())
      
      trainingSum <- shiny$reactiveVal(0)
      traitSum <- shiny$reactiveVal(0)
      positionSum <- shiny$reactiveVal(0)
      footSum <- shiny$reactiveVal(0)
      
      totalCost <- shiny$reactive({
        sum(
          trainingSum(),
          traitSum(),
          positionSum(),
          footSum()
        )
      })
      
      
      #### OUTPUT SERVER ####
      playerInfo$server(id = "info", updated, playerData)
      playerHistory$server(id = "history", updated, playerData)
      training$server(id = "training", trainingSum, playerData)
      traits$server(id = "traits", traitSum, playerData)
      positions$server(id = "positions", positionSum, playerData, session)
      footedness$server(id = "footedness", footSum, playerData)
      
      output$cost <- shiny$renderText({
        paste0("The total cost is: ", totalCost())
      })
      
      #### OBSERVERS ####
      
    }
  })
}
