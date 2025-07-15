box::use(
  bslib,
  dplyr,
  lubridate[
    now,
  ],
  reactable[
    reactable, 
  ],
  scales[dollar],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stringr[
    str_remove, 
    str_to_lower, 
    str_to_upper,
  ],
  tidyr[pivot_longer],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/get[
    getActivePlayer, 
    getPlayer, 
  ],
  app/logic/db/logFunctions[logBankTransaction],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updatePlayerData, updateTPE],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
  app/logic/player/playerHistory,
  app/logic/player/playerInfo,
  app/view/bank/footedness,
  app/view/bank/positions,
  app/view/bank/training,
  app/view/bank/traits,
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
          bslib$layout_column_wrap(
            width = 1 / 2,
            playerInfo$ui(ns("info")),
            playerHistory$ui(ns("history"))
          ),
          bslib$card(
            bslib$card_header(
              shiny$h3("Player Store Purchases")
            ),
            bslib$card_body(
              training$ui(ns("training")),
              traits$ui(ns("traits")),
              if (playerData()$pos_gk != 20) {
                positions$ui(ns("positions"))
              },
              footedness$ui(ns("footedness"))
            )
          ),
          bslib$layout_column_wrap(
            width = 1 / 2,
            shiny$uiOutput(ns("cost")),
            shiny$actionButton(
              ns("verifyPurchase"),
              label = "Verify purchase",
              class = "primary-button"
            )
          ) |> 
            shiny$div(class = "frozen-bottom")
        )
        
      })
      
      #### REACTIVES ####
      playerData <- shiny$reactive({
        getActivePlayer(auth$uid) |> 
          getPlayer()
      }) |> 
        shiny$bindEvent(updated())
      
      trainingSum <- shiny$reactiveVal(0)
      traitSum <- shiny$reactiveVal(0)
      positionSum <- shiny$reactiveVal(0)
      footSum <- shiny$reactiveVal(0)
      
      totalCost <- shiny$reactive({
        sum(
          trainingSum(),
          traitSum(),
          positionSum(),
          footSum()
        )
      })
      
      
      #### OUTPUT SERVER ####
      playerInfo$server(id = "info", updated, playerData)
      playerHistory$server(id = "history", updated, playerData)
      inputTrain <- training$server(id = "training", trainingSum, playerData)
      inputTrait <- traits$server(id = "traits", traitSum, playerData)
      inputPos <- positions$server(id = "positions", positionSum, playerData, session)
      inputFoot <- footedness$server(id = "footedness", footSum, playerData)
      
      output$cost <- shiny$renderUI({
        shiny$tagList(
          paste0(
            "Your bank balance: ", 
            playerData()$bankBalance |> 
              dollar()
          ) |> 
            shiny$p(),
          paste0(
            "Total cost: ", 
            totalCost() |> 
              dollar()
          ) |> 
            shiny$p(),
        )
      })
      
      #### OBSERVERS ####
      shiny$observe({
        if (totalCost() > playerData()$bankBalance | totalCost() <= 0) {
          shiny$updateActionButton(
            inputId = "verifyPurchase", 
            label = "You have exceeded your bank balance!"
          )
          
          disable("verifyPurchase")
        } else {
          shiny$updateActionButton(
            inputId = "verifyPurchase", 
            label = "Verify purchase"
          )
          
          enable("verifyPurchase")
        }
      })
      
      shiny$observe({
        disable("verifyPurchase")
        
        if (totalCost() <= 0) {
          showToast(
            .options = constant$myToastOptions,
            "error",
            "You cannot make a negative purchase."
          )
        } else if (totalCost() > playerData()$bankBalance) {
          showToast(
            .options = constant$myToastOptions,
            "error",
            "Your purchase exceeds your bank balance."
          )
        } else {
          currentPos <- 
            playerData() |> 
            dplyr$select(pos_gk:pos_st) |> 
            pivot_longer(
              cols = dplyr$everything(),
              names_to = "position",
              values_to = "xp"
            ) |> 
            dplyr$mutate(
              position = str_remove(position, "pos_") |> str_to_upper()
            )
          
          purchaseSummary <-
            dplyr$tibble(
              attribute =
                c(
                  "traits", "left foot", "right foot",
                  inputPos$primary, inputPos$secondary,
                  inputPos$unusedPositions,
                  "tpe"
                ),
              old =
                c(
                  playerData()$traits, playerData()$`left foot`,
                  playerData()$`right foot`, 
                  currentPos$xp[currentPos$position %in% inputPos$primary],
                  currentPos$xp[currentPos$position %in% inputPos$secondary],
                  currentPos$xp[currentPos$position %in% inputPos$unusedPositions],
                  "0"
                ),
              new = 
                c(
                  paste0(inputTrait$traits, collapse = constant$traitSep),
                  inputFoot$left |> as.character(), 
                  inputFoot$right |> as.character(),
                  rep("20", times = length(inputPos$primary)), 
                  rep("15", times = length(inputPos$secondary)), 
                  rep("0", times = length(inputPos$unusedPositions)),
                  inputTrain$individualTraining |> as.character()
                )
            ) |> 
            dplyr$filter(new != old)
          
          if (purchaseSummary |> nrow() == 0) {
            showToast(
              .options = constant$sslToastOptions,
              "warning",
              "You have not purchased anything."
            )
          } else {
            shiny$showModal(
              shiny$modalDialog(
                title = "Verify your purchase",
                reactable(purchaseSummary),
                easyClose = FALSE,
                footer = shiny$tagList(
                  shiny$modalButton("Cancel"),
                  shiny$actionButton(ns("confirmUpdate"), "Confirm purchase")
                )
              )
            )
          }
        }
      }) |> 
        shiny$bindEvent(
          input$verifyPurchase
        )
      
      shiny$observe({
        shiny$removeModal()
        
        recentPurchase <- 
          portalQuery(
            "SELECT time
            FROM banktransactions
            WHERE uid = ?uid
            ORDER BY time DESC
            LIMIT 1;",
            uid = auth$uid
          )
        
        if ((now() |> as.numeric()) - recentPurchase$time < 60) {
          showToast(
            .options = constant$sslToastOptions,
            "warning",
            "You have made a purchase recently. In order to prevent duplicated purchases, 
            wait at least 1 minute between purchases."
          )
        } else {
          tryCatch({
            currentPos <- 
              playerData() |> 
              dplyr$select(pos_gk:pos_st) |> 
              pivot_longer(
                cols = dplyr$everything(),
                names_to = "position",
                values_to = "xp"
              ) |> 
              dplyr$mutate(
                position = str_remove(position, "pos_") |> str_to_upper()
              )
            
            purchaseSummary <-
              dplyr$tibble(
                attribute =
                  c(
                    "traits", "left foot", "right foot",
                    paste("pos_", inputPos$primary |> str_to_lower(), sep = ""),
                    paste("pos_", inputPos$secondary |> str_to_lower(), sep = ""),
                    paste("pos_", inputPos$unusedPositions |> str_to_lower(), sep = ""),
                    "tpe"
                  ),
                old =
                  c(
                    playerData()$traits, playerData()$`left foot`,
                    playerData()$`right foot`, 
                    currentPos$xp[currentPos$position %in% inputPos$primary],
                    currentPos$xp[currentPos$position %in% inputPos$secondary],
                    currentPos$xp[currentPos$position %in% inputPos$unusedPositions],
                    "0"
                  ),
                new =
                  c(
                    paste0(inputTrait$traits, collapse = constant$traitSep),
                    inputFoot$left |> as.character(), 
                    inputFoot$right |> as.character(),
                    rep("20", times = length(inputPos$primary)), 
                    rep("15", times = length(inputPos$secondary)), 
                    rep("0", times = length(inputPos$unusedPositions)),
                    inputTrain$individualTraining |> as.character()
                  )
              ) |> 
              dplyr$filter(new != old & attribute != "tpe")
            
            if (purchaseSummary |> nrow() > 0) {
              updatePlayerData(
                uid = auth$uid,
                pid = playerData()$pid,
                updates = purchaseSummary
              )  
            }
            
            if (inputTrain$individualTraining > 0) {
              
              ## Logs and updates the tpe
              updateTPE(
                uid = auth$uid,
                pid = playerData()$pid,
                tpe = dplyr$tibble(
                  source = "Individual Training",
                  tpe = inputTrain$individualTraining
                )
              )
              
              ## Logs and updates the purchased TPE
              portalQuery(
                query = 
                  "UPDATE playerdata 
                  SET purchasedTPE = purchasedTPE + ?tpe 
                  WHERE pid = ?pid;",
                tpe = inputTrain$individualTraining,
                pid = playerData()$pid,
                type = "set"
              )
            }
            
            logBankTransaction(
              uid = auth$uid,
              pid = playerData()$pid,
              source = "Store Purchase",
              transaction = -totalCost()
            )
            
            updated(updated() + 1)
            
            showToast(
              .options = constant$sslToastOptions,
              "success",
              "You have successfully made a purchase!"
            )
          }, error = function(e) {
            message("Error executing query: ", e)
            
            showToast(
              .options = constant$sslToastOptions,
              "error",
              paste(
                "Something is wrong, please notify the BoD with the 
                following error message: \n",
                e$message
              )
            )
          })
        }
        
        enable("verifyPurchase")
        
      }) |> 
        shiny$bindEvent(
          input$confirmUpdate,
          ignoreInit = TRUE
        )
    }
  })
}
