box::use(
  bslib,
  dplyr,
  reactable[
    getReactableState,
    reactable,
    reactableOutput,
    renderReactable
  ],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stats[setNames],
  stringr[
    str_to_lower, 
  ],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/get[getUnapprovedPlayers],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[approvePlayer],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
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
          bslib$card(
            bslib$card_header(
              shiny$h3("Player Approval")
            ),
            bslib$card_body(
              shiny$p(
                "Check the unapproved players for inappropriate names or renders.
                If the are deemed ok, select the player row you want to approve and
                click on Approve selected player."
              ), 
              reactableOutput(ns("needApproval")),
              shiny$br(),
              shiny$br(),
              shiny$actionButton(ns("goApprove"), "Approve selected player")
            )
          )
        )
      })
      
      #### REACTIVES ####
      data <- shiny$reactive({
        getUnapprovedPlayers()
      }) |> 
        shiny$bindEvent(updated())
      
      #### OUTPUT SERVER ####
      output$needApproval <- renderReactable({
        data() |> 
          reactable(
            selection = "single",
            onClick = "select"
          )
      })
      
      #### OBSERVERS ####
      shiny$observe({
        selected <- getReactableState("needApproval", "selected")
        shiny$req(selected)
        
        disable("goApprove")
        
      tryCatch({
        player <- data()[selected, ]
        
        approvePlayer(data = player, uid = auth$uid)
        
        showToast(
          .options = constant$sslToastOptions,
          "success",
          "The manager assignments has been updated"
        )
        
        updated(updated() + 1)
          
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "Something went wrong, contact Canadice"
          )
          
          message("Error updating the manager assignments: ", e)
          
        }, finally = enable("goApprove"))
          
      }) |> 
        shiny$bindEvent(
          input$goApprove
        )
    }
  })
}
