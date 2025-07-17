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
              shiny$uiOutput(ns("schedule")),
              shiny$br(),
              shiny$br(),
              shiny$actionButton(ns("saveGame"), "Save edits")
            )
          )
        )
      })
    } 
    
    #### REACTIVES ####
    schedule <- shiny$reactive({
      data <- 
        getSchedule(
          league = "ALL", 
          season = constant$currentSeason$season
        ) |> 
          dplyr$filter(
            is.na(HomeScore) | is.na(AwayScore)
          )
      
      data |> 
        dplyr$slice(
          seq_len(min(10, nrow(x = data)))
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
    output$schedule <- shiny$renderUI({
      if (schedule() |> nrow() == 0){
        shiny$h5("No edits need to be made")
      } else {
        lapply(
          X = seq_len(nrow(schedule())),
          FUN = function(id) {
            game <- schedule()[id,]
            
            bslib$layout_column_wrap(
              width = 1/7,
              shiny$h5(
                sprintf(
                  "%s MD%s",
                  dplyr$if_else(
                    game$MatchType == 0, 
                    "CUP",
                    dplyr$if_else(
                      game$MatchType == 1, 
                      "MAJOR", 
                      "MINOR"
                    )
                  ),
                  game$MatchDay
                )
              ),
              shiny$selectInput(
                ns(paste0("modalHome", id)), "Home Team",
                choices  = c("TBA", teams()$name),
                selected = game$Home
              ),
              shiny$selectInput(
                ns(paste0("modalAway", id)), "Away Team",
                choices  = c("TBA", teams()$name),
                selected = game$Away
              ),
              shiny$numericInput(
                ns(paste0("modalHScore", id)), "Home Score",
                min = 0,
                max = 20,
                value = game$HomeScore
              ),
              shiny$numericInput(
                ns(paste0("modalAScore", id)), "Away Score",
                min = 0,
                max = 20,
                value = game$AwayScore
              ),
              shiny$checkboxInput(
                ns(paste0("modalEt", id)), "Extra Time",
                value = FALSE
              ),
              shiny$checkboxInput(
                ns(paste0("modalP", id)), "Penalties",
                value = FALSE
              )
            ) 
          }
        )
      }
    })
    
    
    #### OBSERVERS ####
    shiny$observe({
      
      if (schedule() |> nrow() == 0){
        shiny$h5("No edits need to be made")
      } else {
        disable("saveGame")
        
        for (id in seq_len(nrow(schedule()))) {
          game <- schedule()[id, ]
          
          ## Checks if either scores have been updated or if the team have been updated from TBA
          if (
            all(!is.na(input[[paste0("modalHScore", id)]]),
                !is.na(input[[paste0("modalAScore", id)]])) |
            (game$Home == "TBA" & input[[paste0("modalHome", id)]] != "TBA")
          ) {
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
                home   = input[[paste0("modalHome", id)]],
                away   = input[[paste0("modalAway", id)]],
                hscore = input[[paste0("modalHScore", id)]],
                ascore = input[[paste0("modalAScore", id)]],
                et     = dplyr$if_else(input[[paste0("modalEt", id)]], 1, 0),
                p      = dplyr$if_else(input[[paste0("modalP", id)]], 1, 0),
                gid    = game$gid,
                type = "set"
              )
              
              showToast(
                .options = constant$sslToastOptions,
                "success",
                "The game has been updated"
              )
              
            }, error = function(e) {
              showToast(
                .options = constant$sslToastOptions,
                "error",
                "Something went wrong, contact Canadice"
              )
              
              message("Error updating the schedule: ", e)
              
            })
          }
        }
        
        updated(updated() + 1)
        
        enable("saveGame")
      }
    }) |> 
      shiny$bindEvent(
        input$saveGame
      )
      
    
    
  })
}
