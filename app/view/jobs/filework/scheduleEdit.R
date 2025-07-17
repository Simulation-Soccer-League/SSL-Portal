box::use(
  bslib,
  dplyover[across2],
  dplyr,
  reactable[
    colDef,
    getReactableState, 
    reactable, 
    reactableOutput, 
    renderReactable,
    updateReactable, 
  ],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stringr[str_remove_all, str_to_lower, str_to_title],
  tidyr[pivot_longer, replace_na],
)

box::use(
  app/logic/constant,
  app/logic/db/database[indexQuery],
  app/logic/db/get[getOrganizations, getSchedule],
  app/logic/db/login[isBoD, isFileworker, isNonActiveForumUser],
  app/logic/import[importGameData, parseFMdata],
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
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else if (isBoD(auth$usergroup) | isFileworker(auth$usergroup)) {
      output$ui <- shiny$renderUI({
        shiny$tagList(
          bslib$card(
            bslib$card_header(
              shiny$h3("Edit the schedule")
            ),
            bslib$card_body(
              reactableOutput(ns("schedule")),
              shiny$br(),
              shiny$br(),
              shiny$actionButton(ns("editSelected"), "Edit selected game")
            )
          )
        )
      })
    } 
    
    
    
    #### REACTIVES ####
    schedule <- shiny$reactive({
      getSchedule(
        league = "ALL", 
        season = constant$currentSeason$season
      ) |> 
        dplyr$filter(
          is.na(HomeScore) | is.na(AwayScore)
        )
    }) |> 
      shiny$bindEvent(updated())
    
    teams <- shiny$reactive({
      getOrganizations() |> 
        dplyr$filter(ID >= 0) |> 
        dplyr$arrange(name) |> 
        dplyr$add_row(
          dplyr$tibble(
            name = 
              c("Canada", "Benelux", "Eurasia", "Norden", "West Africa",
                "South America", "Oceania", "Pyrenees", "Alpen", 
                "East Europe", "British Isles", "Central America", 
                "East Africa", "Asia", "Central Europe", "USA") |> 
              sort()
          )
        )
    })
    
    #### OUTPUT SERVER ####
    output$schedule <- renderReactable({
      schedule() |> 
        reactable(
          selection = "single",
          onClick = "select",
          defaultPageSize = 10
        )
    })
    
    #### OBSERVERS ####
    shiny$observe({
      selected <- getReactableState("schedule")$selected
      
      shiny$req(selected |> length() == 1)
      
      game <- schedule()[selected, ]
      
      shiny$showModal(
        shiny$modalDialog(
          title = 
            sprintf(
              "Editing Game: %s MD%s",
              dplyr$if_else(
                game$MatchType == 0, 
                "Cup",
                dplyr$if_else(
                  game$MatchType == 1, 
                  "MAJOR", 
                  "MINOR"
                )
              ),
              game$MatchDay
            ),
          
          shiny$selectInput(
            ns("modalHome"), "Home Team",
            choices  = teams()$name,
            selected = game$Home
          ),
          shiny$selectInput(
            ns("modalAway"), "Away Team",
            choices  = teams()$name,
            selected = game$Away
          ),
          shiny$selectInput(
            ns("modalHScore"), "Home Score",
            choices  = 0:20,
            selected = game$HomeScore
          ),
          shiny$selectInput(
            ns("modalAScore"), "Away Score",
            choices  = 0:20,
            selected = game$AwayScore
          ),
          shiny$checkboxInput(
            ns("modalEt"), "Extra Time",
            value = FALSE
          ),
          shiny$checkboxInput(
            ns("modalP"), "Penalties",
            value = FALSE
          ),
          
          footer = shiny$tagList(
            shiny$modalButton("Cancel"),
            shiny$actionButton(ns("saveGame"), "Save Changes")
          ),
          size      = "m",
          easyClose = FALSE
        )
      )
    }) |> 
      shiny$bindEvent(
        input$editSelected
      )
    
    
    shiny$observe({
      selected <- getReactableState("schedule")$selected
      
      shiny$req(selected |> length() == 1)
      
      game <- schedule()[selected, ]
      
      # call your update function
      tryCatch({
        indexQuery(
          "UPDATE schedule 
             SET Home = {home},
                 Away = {away},
                 HomeScore = {hscore},
                 AwayScore = {ascore},
                 ExtraTime = {et},
                 Penalties = {p}
             WHERE gid = {gid};",
          home   = input$modalHome,
          away   = input$modalAway,
          hscore = input$modalHScore,
          ascore = input$modalAScore,
          et     = dplyr$if_else(input$modalEt, 1, 0),
          p      = dplyr$if_else(input$modalP, 1, 0),
          gid    = game$gid,
          type = "set"
        )
        
        showToast(
          .options = constant$sslToastOptions,
          "success",
          "The game has been updated"
        )
        
        updated(updated() + 1)
      }, error = function(e) {
        showToast(
          .options = constant$sslToastOptions,
          "error",
          "Something went wrong, contact Canadice"
        )
        
        message("Error updating the schedule: ", e)
        
      }, finally = shiny$removeModal()
      )
      
    }) |> 
      shiny$bindEvent(input$saveGame)
    
    
  })
}
