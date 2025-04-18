box::use(
  bslib,
  dplyr,
  shiny,
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer, getPlayer],
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
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        shiny$tagList(
          bslib$layout_column_wrap(
            width = 1/6,
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
            if (canRedistribute())
            shiny$actionButton(ns("Update"), label = "UPDATE"),
            shiny$actionButton(ns("Update"), label = "UPDATE"),
            shiny$actionButton(ns("Update"), label = "UPDATE"),
          ),
          player$ui(ns("player"))
        )
      })
      
      #### OUTPUT SERVER ####
      player$server("player", pid = getActivePlayer(auth$uid), updated = updated)
      
      #### REACTIVES ####
      bankedTPE <- shiny$reactive({
        auth$uid |> 
        getActivePlayer() |> 
        getPlayer() |> 
        dplyr$select(tpebank)
      })
    }
  })
}
