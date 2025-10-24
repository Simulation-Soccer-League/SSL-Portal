box::use(
  bslib,
  dplyr,
  reactable[
    colDef, 
    getReactableState,
    reactable, 
    reactableOutput, 
    renderReactable
  ],
  shiny,
  shinyFeedback[showToast],
  shinyjs,
  stringr[
    str_to_upper,
  ],
)

box::use(
  app/logic/constant,
  app/logic/db/get[
    getManagedPlayers,
  ],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
  app/view/player/playerUpdate,
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
    
      
    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      shiny$tagList(
        bslib$card(
          bslib$card_header(
            shiny$h3("Roster Overview")
          ),
          bslib$card_body(
            shiny$p(
              "This page is used by managers to get an overview of their players and be able 
              to regress them. Any highlighted player in red needs to be regressed and if the 
              user is unable to do so, you as a manager can complete their regression. Select 
              the player you want to regress and click on 'Regress' to open up the player 
              attribute view where you are able to regress attributes."
            ),
            reactableOutput(ns("rosterOverview")) |> 
              withSpinnerCustom(height = 100),
            shiny$br(),
            shiny$br(),
            shiny$actionButton(ns("goRegress"), "Regress Player"),
            bslib$card(
              bslib$card_header(
                "Player Regression"
              ),
              bslib$card_body(
                shiny$uiOutput(ns("player"))
              )
            ) |> 
              shiny$div(id = ns("selectedPlayer")) |> 
              shinyjs$hidden()
          )
        )
      )
    })
    
    #### REACTIVES ####
    players <- shiny$reactive({
      getManagedPlayers(auth$uid) |> 
        dplyr$select(
          pid,
          username,
          discord,
          playerStatus,
          userStatus,
          name,
          class,
          created,
          nationality,
          tpe,
          tpebank,
          `left foot`,
          `right foot`,
          pos_st:traits,
          bankBalance,
          `minimum salary`,
          rerollused:timesregressed,
          purchasedTPE,
          affiliate
        ) |> 
        dplyr$mutate(
          affiliate = dplyr$if_else(affiliate == 1, "Major", "Minor") |> 
            factor(levels = c("Major", "Minor")),
          dplyr$across(
            rerollused:redistused,
            ~ dplyr$if_else(.x == 1, "Used", "Not used")
          )
        ) |> 
        dplyr$arrange(affiliate, created) |> 
        dplyr$select(!created)
    }) |> 
      shiny$bindEvent(
        updated()
      )
    
    
    #### OUTPUT SERVER ####
    output$rosterOverview <- renderReactable({
      players() |> 
        dplyr$select(!pid) |> 
        reactable(
          selection = "single",
          onClick = "select",
          groupBy = "affiliate",
          defaultColDef = colDef(header = function(value) {
            str_to_upper(value)
          }),
          rowStyle = function(index) {
            if (players()[index, "tpebank"] < 0) {
              list(background = "#FFCCCB", color = "black")
            }
          }
        )
    })
    
    output$player <- shiny$renderUI({
      selected <- getReactableState("rosterOverview", "selected")
      shiny$req(selected)
      
      selection <- players()[selected, ]
      
      playerUpdate$ui(
        ns(paste0("attributes_", selection$pid))
      )
    })
    
    #### OBSERVERS ####
    
    shiny$observe({
      selected <- getReactableState("rosterOverview", "selected")
      shiny$req(selected)
      
      selection <- players()[selected, ]
      
      if (selection$tpebank < 0) {
        shinyjs$show("selectedPlayer")
        
        playerUpdate$server(
          paste0("attributes_", selection$pid),
          auth = auth,
          updated = updated,
          type = "regress",
          player = selection
        )
      } else {
        shinyjs$hide("selectedPlayer")
        
        showToast(
          .options = constant$sslToastOptions,
          "warning",
          "Selected player does not need to regress."
        )
      }
    }) |> 
      shiny$bindEvent(
        input$goRegress
      )
    
    shiny$observe({
      shinyjs$hide("selectedPlayer")
    }) |> 
      shiny$bindEvent(updated())
  })
}
