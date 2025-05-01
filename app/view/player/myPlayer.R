box::use(
  bslib,
  dplyr,
  shiny,
  shiny.router[change_page],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[eligibleRedist, eligibleReroll],
  app/view/tracker/player,
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
    
    print("myPlayer")
    
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())){
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)){
      # TODO ADD NOTE THAT YOU HAVE NO ACTIVE PLAYER
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else {
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        buttonList <- list(
          if (bankedTPE() < 0) {
            shiny$actionButton(
              ns("Update"), 
              label = tippy("Update", "You do not have any banked TPE to update with.", 
                            theme = "ssl", arrow = TRUE), 
              disabled = TRUE
            )
          } else {
            shiny$actionButton(
              ns("Update"), 
              label = "Update",
              style = paste0("background: ", constant$green)
            )
          },
          if (bankedTPE() < 0) {
            shiny$actionButton(
              ns("Regress"), 
              label = "Regress"
            )
          } else {
            shiny$actionButton(
              ns("Regress"), 
              label = tippy("Regress", "You do not need to regress your player.", 
                            theme = "ssl", arrow = TRUE), 
              disabled = TRUE
            )
          },
          shiny$actionButton(ns("Retire"), label = "Retire", style = paste0("background: ", constant$red)),
          if (eligibleRedist(playerData())){
            shiny$actionButton(
              ns("Redistribute"), 
              label = "Redistribute"
            )
          } else {
          },
          if (eligibleReroll(playerData())){
            shiny$actionButton(
              ns("Reroll"), 
              label = "Reroll"
            )
          } else {
          }
        )
        
        shiny$tagList(
          bslib$layout_column_wrap(
            width = 1/length(buttonList),
            buttonList
          ),
          shiny$br(),
          player$ui(ns("player"))
        )
      })
      
      #### OUTPUT SERVER ####
      player$server("player", pid = getActivePlayer(auth$uid), updated = updated)
      
      #### REACTIVES ####
      bankedTPE <- shiny$reactive({
        playerData() |>  
        dplyr$select(tpebank)
      })
      
      playerData <- shiny$reactive({
        print(updated())
        
        getActivePlayer(auth$uid) |> 
          getPlayer()
      }) |> 
        shiny$bindEvent(updated())
      
      #### OBSERVERS ####
      shiny$observe({
        change_page("myPlayer/update")
      }) |> 
        shiny$bindEvent(input$Update)
      
      shiny$observe({
        change_page("myPlayer/reroll")
      }) |> 
        shiny$bindEvent(input$Reroll)
      
      shiny$observe({
        change_page("myPlayer/redistribute")
      }) |> 
        shiny$bindEvent(input$Redistribute)
      
      shiny$observe({
        change_page("myPlayer/regress")
      }) |> 
        shiny$bindEvent(input$Regress)
    }
  })
}
