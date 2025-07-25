box::use(
  bslib,
  dplyr,
  purrr[is_empty],
  shiny,
  shiny.router[change_page],
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stringr[str_extract_all],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/discord[sendRetiredPlayer],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/db/updateFunctions[updateTPE],
  app/logic/player/playerChecks[
    completedAC,
    completedTC, 
    eligibleRedist, 
    eligibleReroll
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
    
    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      buttonList <- list(
        if (completedAC(playerData()$pid)) {
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
        if (completedTC(playerData()$pid)) {
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
        shiny$actionButton(
          ns("Retire"), 
          label = "Retire", 
          style = paste0("background: ", constant$red)
        ),
        if (eligibleRedist(playerData())) {
          shiny$actionButton(
            ns("Redistribute"), 
            label = "Redistribute"
          )
        } else {
          NULL
        },
        if (eligibleReroll(playerData())) {
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
      }, error = function(e) {
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
      }, error = function(e) {
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
    
    shiny$observe({
      shiny$showModal(
        shiny$modalDialog(
          "Are you sure you want to retire? This is a permanent decision and 
          you may not un-retire after going through with the retirement.",
          title = "Retirement",
          footer = shiny$tagList(
            shiny$modalButton("No, go back"),
            shiny$actionButton(
              inputId = ns("confirmRetirement1"),
              label = "Yes, I am fully aware of the results of this decision and want to continue!"
            )
          ),
          easyClose = FALSE
        )
      )
    }) |> 
      shiny$bindEvent(input$Retire)
    
    shiny$observe({
      shiny$removeModal()
      
      shiny$showModal(
        shiny$modalDialog(
          "Are you really sure? This is your final warning, you cannot 
          revert your decision if you continue.",
          title = "Retirement",
          footer = shiny$tagList(
            shiny$modalButton("No, go back"),
            shiny$actionButton(
              inputId = session$ns("confirmRetirement2"),
              label = "Yes, I want to permanently retire!"
            )
          ),
          easyClose = FALSE
        )
      )
    }) |> 
      shiny$bindEvent(input$confirmRetirement1)
    
    shiny$observe({
      shiny$removeModal()
      
      tryCatch({
        # Begin the transaction
        portalQuery(
          query = "START TRANSACTION;",
          type = "set"
        )
        
        portalQuery(
          query = "UPDATE playerdata 
          SET status_p = 2 
          WHERE pid = {pid};",
          pid = getActivePlayer(auth$uid),
          type = "set"
        )
        
        sendRetiredPlayer(playerData())
      }, error = function(e) {
        # Rollback the transaction if any error occurs
        portalQuery(
          query = "ROLLBACK;",
          type = "set"
        )
        
        message("Error updating banktransactions, transaction rolled back: ", e$message)
      })
      
    }) |> 
      shiny$bindEvent(
        input$confirmRetirement2
      )
    
  })
}
