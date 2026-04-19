box::use(
  bslib,
  dplyr,
  shiny,
  shinyFeedback[showToast],
  sortable[add_rank_list, bucket_list],
  stats[setNames],
  stringr[
    str_remove,
    str_split,
    str_to_lower,
    str_to_upper,
  ],
  tidyr[pivot_longer],
)

box::use(
  app/logic/constant,
  app/logic/db/get[
    getPlayer,
    getPlayerContract,
    getPlayers,
  ],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updatePlayerData],
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
    
    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      shiny$tagList(
        bslib$card(
          bslib$card_header(
            shiny$h3("Edit Contract")
          ),
          bslib$card_body(
            shiny$p(
              "This page is used to edit a player's contract, such as moving rosters or managing extensions or releases."
            ),
            bslib$layout_column_wrap(
              width = 1 / 3,
              shiny$div(
                shiny$selectInput(
                  ns("selectedPlayer"),
                  "Select a player", 
                  choices = NULL
                ),
                shiny$actionButton(
                  ns("update"),
                  "Update Player",
                  style = paste0("background: ", constant$green)
                )
              ),
              shiny$div(
                shiny$h4("Organization Information"),
                shiny$selectInput(
                  ns("organization"),
                  "Select organization",
                  choices = NULL
                ),
                shiny$selectInput(
                  ns("affiliate"),
                  "Select affiliate",
                  choices = c("Major" = 1, "Minor" = 2)
                )
              ),
              shiny$div(
                shiny$h4("Latest Contract"),
                shiny$textInput(
                  ns("url"),
                  "Link",
                  value = ""
                ),
                bslib$layout_column_wrap(
                  width = 1 / 2,
                  shiny$numericInput(
                    ns("start"), 
                    "Start Season",
                    min = constant$currentSeason$season - 2,
                    max = constant$currentSeason$season + 1,
                    value = NULL
                  ),
                  shiny$numericInput(
                    ns("end"), 
                    "End Season",
                    min = constant$currentSeason$season,
                    max = constant$currentSeason$season + 3,
                    value = NULL
                  ),
                  shiny$checkboxInput(
                    ns("release"),
                    label = "Released?",
                    value = FALSE
                  ),
                  shiny$checkboxInput(
                    ns("inactive"),
                    label = "Inactive Status?",
                    value = FALSE
                  )
                )
              )
            )
          )
        )
      )
    })
    
    #### REACTIVES ####
    playerData <- shiny$reactive({
      shiny$req(input$selectedPlayer)
      
      pid <- input$selectedPlayer
      
      getPlayerContract(pid)
    }) |> 
      shiny$bindEvent(
        input$selectedPlayer,
        updated()
      )
    
    allNames <- shiny$reactive({
      getPlayers(active = TRUE) |> 
        dplyr$select(
          name, pid, username, team, status_p
        )
    })
    
    organizations <- shiny$reactive({
      constant$organizations |> 
        dplyr$select(id = ID, name = name, abbr = abbreviation) |> 
        dplyr$filter(!is.na(name)) |> 
        dplyr$distinct()
    })
    
    
    #### OUTPUT SERVER ####
    
    #### OBSERVERS ####
    
    ## Populating player selector
    shiny$observe({
      shiny$req(allNames())
      
      names <- allNames() |> 
        dplyr$arrange(name)
      
      namedVector <- names$pid
      
      names(namedVector) <- names$name
      
      shiny$updateSelectInput(
        inputId = "selectedPlayer", 
        choices = namedVector
      )
    })
    
    ## Populating organization and affiliate    
    shiny$observe({
      shiny$req(organizations(), playerData())
      
      orgVector <- setNames(organizations()$id, organizations()$name)
      currentOrg <- playerData()$organization
      
      shiny$updateSelectInput(
        inputId = "organization", 
        choices = orgVector,
        selected = orgVector[currentOrg]
      )
      
      shiny$updateSelectInput(
        inputId = "affiliate", 
        selected = playerData()$affiliate
      )
    })
    
    ## Populating player information
    # Cannot populate positions as bucket_list cannot update
    shiny$observe({
      shiny$req(playerData())
      
      shiny$updateTextInput(
        session,
        inputId = "url",
        value = playerData()$contractURL |> unique()
      )
      
      shiny$updateNumericInput(
        session,
        inputId = "start",
        value = playerData()$firstSeason |> unique()
      )
      
      shiny$updateNumericInput(
        session,
        inputId = "end",
        value = playerData()$lastSeason |> unique()
      )
      
      shiny$updateCheckboxInput(
        session,
        inputId = "release",
        value = (playerData()$released |> unique()) == 1
      )
      
      shiny$updateCheckboxInput(
        session,
        inputId = "inactive",
        value = (playerData()$ia |> unique()) == 1
      )
      
    }) |> 
      shiny$bindEvent(playerData())
    
    ## Edits player
    shiny$observe({
      summary <- 
        dplyr$tibble(
          team = input$organization,
          affiliate = input$affiliate,
          `left foot` = input$left,
          `right foot` = input$right,
          traits = paste0(input$traits, collapse = constant$traitSep),
          render = input$render
        )
      
      for (pos in names(constant$positionsGK)) {
        column <- paste0("pos_", str_to_lower(pos))
        
        summary <- 
          summary |> 
          cbind(
            dplyr$tibble(
              !!column := 
                dplyr$if_else(
                  any(input$primary == pos), 
                  20, 
                  dplyr$if_else(
                    any(input$secondary == pos), 
                    15, 
                    0
                  )
                )
            )
          )
      }
      
      old <- 
        playerData() |> 
        dplyr$select(
          team = organization, affiliate,
          `left foot`, `right foot`,
          traits, render,
          pos_st:pos_gk
        ) |> 
        dplyr$mutate(
          team = organizations()$id[organizations()$name == team]
        ) |> 
        pivot_longer(
          dplyr$everything(),
          values_transform = as.character
        )
      
      update <- 
        dplyr$left_join(
          summary |> 
            pivot_longer(
              dplyr$everything(),
              values_transform = as.character
            ),
          old,
          by = "name"
        ) |> 
        dplyr$rename(attribute = name, old = value.y, new = value.x) |> 
        dplyr$filter(old != new)
      
      if (nrow(update) != 0) {
        tryCatch({
          updatePlayerData(uid = auth$uid, pid = playerData()$pid, updates = update)
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "The player has been updated!"
          )
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            paste(
              "Something is wrong, please notify the BoD with the 
                following error message: \n",
              e$message
            )
          )
          
          message("Error executing query: ", e)
        })
      } else {
        showToast(
          .options = constant$sslToastOptions,
          "warning",
          "Nothing has been changed in the build."
        )
      }
      
    }) |> 
      shiny$bindEvent(input$update)
    
  })
}
