box::use(
  bslib,
  shiny,
)

box::use(
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer],
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
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())){
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)){
      # TODO ADD NOTE THAT YOU HAVE NO ACTIVE USER
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else {
      output$ui <- shiny$renderUI({
        shiny$tagList(
          bslib$layout_column_wrap(
            width = 1/6,
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
            shiny$actionButton(session$ns("Update"), label = "UPDATE"),
          ),
          player$ui(session$ns("player"))
        )
        
        
      })
      
      player$server("player", pid = getActivePlayer(auth$uid), updated = updated)
    }
  })
}
