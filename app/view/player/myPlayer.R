box::use(
  bslib,
  dplyr,
  lubridate[
    now,
  ],
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
  app/logic/db/database[portalQuery, updateTPE],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/db/updateFunctions[retirePlayer],
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
        shiny$actionButton(
          ns("Retire"), 
          label = "Retire", 
          style = paste0("background: ", constant$red)
        ),
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
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 8fr 1fr"),
          "",
          bslib$layout_column_wrap(
            width = 1 / length(buttonList),
            !!!unname(buttonList)
          ),
          ""
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
        
        recentPost <- 
          portalQuery(
            "SELECT time
            FROM tpehistory
            WHERE uid = {uid} AND pid = {pid} AND source = 'Activity Check'
            ORDER BY time DESC
            LIMIT 1;",
            uid = auth$uid,
            pid = playerData()$pid
          )
        
        if (recentPost |> nrow() == 0) {
          recentPost <- dplyr$tibble(time = 0)
        }
        
        if ((now() |> as.numeric()) - recentPost$time < 240) {
          showToast(
            .options = constant$sslToastOptions,
            "warning",
            "You have (accidentally we hope) clicked the AC button more times than allowed.
            You have already claimed your Activity Check for this week."
          )
        } else {
          tpe <- 
            dplyr$tibble(
              pid = playerData()$pid,
              source = "Activity Check",
              tpe = 6
            )
          
          updateTPE(uid = auth$uid, tpeData = tpe)
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully claimed your Activity Check for the week!"
          )
        }
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
        
        recentPost <- 
          portalQuery(
            "SELECT time
            FROM tpehistory
            WHERE uid = {uid} AND pid = {pid} AND source LIKE '%Training Camp'
            ORDER BY time DESC
            LIMIT 1;",
            uid = auth$uid,
            pid = playerData()$pid
          )
        
        if (recentPost |> nrow() == 0) {
          recentPost <- dplyr$tibble(time = 0)
        }
        
        if ((now() |> as.numeric()) - recentPost$time < 240) {
          showToast(
            .options = constant$sslToastOptions,
            "warning",
            "You have (accidentally we hope) clicked the TC button more times than allowed.
            You have already claimed your Training Camp for this season."
          )
        } else {
          class <- playerData()$class |> 
            str_extract_all(pattern = "[0-9]+") |> 
            as.numeric()
          
          age <- constant$currentSeason$season - class
          
          tpe <- 
            dplyr$tibble(
              pid = playerData()$pid,
              source = paste0("S", constant$currentSeason$season, " Training Camp"),
              tpe = dplyr$case_when(
                age <= 1 ~ 24,
                age <= 4 ~ 18,
                age <= 7 ~ 12,
                TRUE     ~  6
              )
            )
          
          updateTPE(uid = auth$uid, tpeData = tpe)
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully claimed your Training Camp for the season!"
          )
        }
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
          you may not un-retire after going through with the retirement. NOTE
          THAT IF YOU RETIRE PRIOR TO THE CHANGE OF SEASON YOU WILL RETIRE AT 
          THE END OF THE CURRENT SEASON.",
          title = "Really?",
          footer = shiny$tagList(
            shiny$modalButton("No, go back"),
            shiny$actionButton(
              inputId = ns("confirmRetirement1"),
              label = "Yes, I am fully aware of the results of this decision!"
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
          revert your decision if you continue. IF IT'S NOT THE NEW SEASON
          AND YOU WANT A CORPSE SEASON FOR YOUR PLAYER, TURN BACK NOW!",
          title = "Really really?",
          footer = shiny$tagList(
            shiny$actionButton(
              inputId = session$ns("confirmRetirement2"),
              label = "Yes, I want to permanently retire!"
            ),
            shiny$modalButton("No, go back")
          ) |> 
            shiny$div(align = "left"),
          easyClose = FALSE
        )
      )
    }) |> 
      shiny$bindEvent(input$confirmRetirement1)
    
    shiny$observe({
      shiny$removeModal()
      
      result <- 
        tryCatch({
          retirePlayer(data = playerData(), uid = auth$uid)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully retired your player. We hope to see you recreate in the near future!"
          )
          
          change_page("")
        }, error = function(e) {
          message("Error executing query: ", e)
          
          showToast(
            .options = constant$sslToastOptions,
            "error",
            paste("Something is wrong, please notify the BoD with the following error message: \n",
                  e$message)
          )
        })
      
    }) |> 
      shiny$bindEvent(
        input$confirmRetirement2
      )
    
  })
}
