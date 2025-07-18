box::use(
  bslib,
  dplyr,
  purrr[is_empty],
  rlang[`!!!`],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  shiny.router[change_page],
  stringr[str_extract_all],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updateTPE],
  app/logic/player/playerChecks[
    completedAC,
    completedTC, 
    eligibleRedist, 
    eligibleReroll, 
    hasActivePlayer
  ],
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
        buttonList <- list(
          if (completedAC(playerData()$pid)){
            shiny$actionButton(
              ns("AC"),
              label = 
                tippy(
                  "Activity Check", 
                  "You have already completed this week's Activity Check.", 
                  theme = "ssl", 
                  arrow = TRUE
                ),
              disabled = TRUE
            )
          } else {
            shiny$actionButton(
              ns("AC"),
              label = 
                tippy(
                  "Activity Check", 
                  "Click here to claim your weekly Activity Check TPE", 
                  theme = "ssl", 
                  arrow = TRUE
                ),
              style = paste0("background: ", constant$blue)
            )
          },
          if (completedTC(playerData()$pid)){
            NULL
          } else {
            shiny$actionButton(
              ns("TC"),
              label = 
                tippy(
                  "Training Camp", 
                  "Click here to claim your seasonal Training Camp TPE", 
                  theme = "ssl", 
                  arrow = TRUE
                ),
              style = paste0("background: ", constant$blue)
            )
          }, 
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
            NULL
          },
          if (eligibleReroll(playerData())){
            shiny$actionButton(
              ns("Reroll"), 
              label = "Reroll"
            )
          } else {
            NULL
          }
        )
        
        ## Removes empty buttons in the list
        buttonList <- 
          buttonList[!sapply(X = buttonList, FUN = is_empty)]
        
        shiny$tagList(
          bslib$layout_column_wrap(
            width = 1 / min(4, length(buttonList)),
            !!!unname(buttonList)
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
      
      shiny$observe({
        
        tryCatch({
          disable(ns("AC"))
          
          tpe <- 
            dplyr$tibble(
              source = "Activity Check",
              tpe = 6
            )
          
          updateTPE(uid = auth$uid, pids = playerData()$pid, tpe = tpe)
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully claimed your Activity Check for the week!"
          )
        }, error = function(e){
          message("Error executing query: ", e)
          
          showToast(
            .options = constant$sslToastOptions,
            "error",
            paste("Something is wrong, please notify the BoD with the following error message: \n",
                  e$message)
          )
          
          enable(ns("AC"))
        })
        
      }) |> 
        shiny$bindEvent(input$AC)
      
      shiny$observe({
        
        tryCatch({
          disable(ns("TC"))
          
          class <- playerData()$class |> 
            str_extract_all(pattern = "[0-9]+") |> 
            as.numeric()
          
          age <- constant$currentSeason$season - class
          
          tpe <- 
            dplyr$tibble(
              source = paste0("S", constant$currentSeason$season, " Training Camp"),
              tpe = dplyr$case_when(
                age <= 1 ~ 24,
                age <= 4 ~ 18,
                age <= 7 ~ 12,
                TRUE     ~  6
              )
            )
          
          updateTPE(uid = auth$uid, pids = playerData()$pid, tpe = tpe)
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully claimed your Training Camp for the season!"
          )
        }, error = function(e){
          message("Error executing query: ", e)
          
          showToast(
            .options = constant$sslToastOptions,
            "error",
            paste("Something is wrong, please notify the BoD with the following error message: \n",
                  e$message)
          )
          
          enable(ns("TC"))
        })
        
      }) |> 
        shiny$bindEvent(input$TC)
    }
  })
}
