box::use(
  bslib,
  dplyr,
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
  app/logic/db/get[getManagers, getPlayers, ],
  app/logic/db/login[isNonActiveForumUser],
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
              shiny$h3("Organization Overview")
            ),
            bslib$card_body(
              shiny$uiOutput(ns("orgTable")),
              shiny$br(),
              shiny$br(),
              shiny$actionButton(ns("saveManagers"), "Save assignments")
            )
          )
        )
      })
      
      #### REACTIVES ####
      data <- shiny$reactive({
        getManagers()
      }) |> 
        shiny$bindEvent(updated())
      
      allUsers <- shiny$reactive({
        getPlayers(active = TRUE) |> 
          dplyr$select(username, uid) |> 
          dplyr$arrange(username |> 
                          str_to_lower()) |>
          dplyr$distinct() |> 
          dplyr$filter(!is.na(username))
      })
      
      
      #### OUTPUT SERVER ####
      output$orgTable <- shiny$renderUI({
        lapply(
          X = data()$id,
          FUN = function(id) {
            org <- data()[data()$id == id, ]
            userVector <- setNames(allUsers()$uid, allUsers()$username)
            
            bslib$layout_column_wrap(
              width = 1 / 4,
              shiny$h5(org$name),
              shiny$selectInput(
                ns(paste0("manager_", id)), "Organizational Manager",
                choices  = c(" ", userVector),
                selected = org$orgManager
              ),
              shiny$selectInput(
                ns(paste0("assistant1_", id)), "Assistant Manager",
                choices  = c(" ", userVector),
                selected = org$assManager1
              ),
              shiny$selectInput(
                ns(paste0("assistant2_", id)), "Assistant Manager",
                choices  = c(" ", userVector),
                selected = org$assManager2
              )
            ) 
          }
        )
      })
      
      
      #### OBSERVERS ####
      shiny$observe({
        disable("saveManagers")
        
      tryCatch({
        for (id in data()$id){
          org <- data()[data()$id == id, ]
          
          # Pre-process the values: convert empty strings to NA
          orgManager  <- 
            dplyr$if_else(
              input[[paste0("manager_", id)]] == "", 
              NA_integer_, 
              input[[paste0("manager_", id)]] |> 
                as.integer()
            )
          assManager1 <- 
            dplyr$if_else(
              input[[paste0("assistant1_", id)]] == "", 
              NA_integer_, 
              input[[paste0("assistant1_", id)]] |> 
                as.integer()
            )
          assManager2 <- 
            dplyr$if_else(
              input[[paste0("assistant2_", id)]] == "", 
              NA_integer_, 
              input[[paste0("assistant2_", id)]] |> 
                as.integer()
            )
        
          portalQuery(
            query = 
              "UPDATE managers
               SET orgManager = {orgManager},
                   assManager1 = {assManager1},
                   assManager2 = {assManager2}
               WHERE orgID = {id};",
            orgManager  = orgManager,
            assManager1 = assManager1,
            assManager2 = assManager2,
            id          = id,
            type        = "set"
          )
          
        }
        
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
          
        }, finally = enable("saveManagers"))
          
      }) |> 
        shiny$bindEvent(
          input$saveManagers
        )
    }
  })
}
