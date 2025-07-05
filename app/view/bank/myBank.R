box::use(
  bslib,
  dplyr,
  lubridate[
    as_datetime, 
  ],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  scales[label_currency],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  sortable[add_rank_list, bucket_list],
  stringr[str_detect, str_remove, str_split, str_to_upper],
  tidyr[pivot_longer]
)

box::use(
  app/logic/constant,
  app/logic/db/get[
    getActivePlayer, 
    getPlayer, 
    getTpeHistory, 
    getBankHistory, 
    getUpdateHistory
  ],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updateTPE],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
  app/logic/player/playerHistory,
  app/logic/player/playerInfo,
  app/logic/ui/spinner[withSpinnerCustom],
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
              shiny$tagList(
                shiny$p(
                  "You may purchase at most 18 TPE from individual training per season. Each TPE purchased
            cost $425 000 for a total cost of $7.65 million for the entire 18 TPE."
                ),
                shiny$sliderInput(
                  ns("individualTraining"),
                  label = "Pull the slider to the amount of TPE you want to purchase:",
                  min = 0,
                  max = 18 - playerData()$purchasedTPE,
                  value = 0,
                  step = 1,
                  ticks = FALSE,
                  width = "100%"
                )
              ),
              shiny$tagList(
                shiny$p(
                  paste(
                    "Each new trait costs $3 million and your player can have a maximum of 7 traits, including the 2 you started with.",
                    "The traits are separated into seven different categories depending on what part of the play they affect. Note that 
              some of the traits will negatively impact your player if their attributes does not match. If you notice a disabled 
              trait in the list, it is not compatible with one or more of your current traits. The full list of incompatible traits 
              can be found in the", shiny$tags$a("Player Handbook.", href = "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit#", target = "_blank"),
                    "If you want to remove an already selected player trait, you can purchase this item ($0.5 million)."
                  ) |> 
                    shiny$HTML()
                ),
                shiny$checkboxGroupInput(
                  ns("traits"), 
                  paste("Change your traits"), 
                  choices = constant$traits |> unlist(use.names = FALSE), 
                  selected = currentTraits()
                ) |>  
                  shiny$div(class = "multicol"),
                shiny$tags$script(
                  paste(
                    "Shiny.addCustomMessageHandler('disableCheckbox', function(checkboxId) {
                      if (typeof checkboxId === 'string') {
                        checkboxId = [checkboxId]; // Convert single string to array
                      }
                      var checkboxes = document.getElementsByName('", ns('traits'), "');
                      for (var i = 0; i < checkboxes.length; i++) {
                        checkboxes[i].disabled = false; // Disable specific checkboxes
                      }
                      for (var i = 0; i < checkboxes.length; i++) {
                        for (var j = 0; j < checkboxId.length; j++) {
                          if(checkboxes[i].value == checkboxId[j]){
                            checkboxes[i].disabled = true; // Disable specific checkboxes
                          } else {
                            
                          }
                        }
                      }
                    });", sep = "") |> shiny$HTML()
                )
              ),
              if(playerData()$pos_gk != 20){
                shiny$tagList(
                  shiny$p(
                    paste(
                      "At creation, a player starts with one primary and two secondary positions. With purchases in the Player Store you are able to switch, 
                      upgrade or add positions to your player.",
                      "The items available are:",
                        shiny$tags$ul(
                          shiny$tags$li("Position Transfer ($5 million): A transfer consists of swapping two positions between any boxes."),
                          shiny$tags$li("Position Upgrade ($7.5 million): An upgrade consists of moving a position from the secondary to primary box."),
                          shiny$tags$li("Position Addition ($12.5 million): An addition consists of moving a 'new' position to the secondary position box. If you move a position to the primary box this is a Addition + Upgrade.")
                        )
                    ) |> 
                      shiny$HTML()
                  ),
                  bucket_list(
                    header = NULL,
                    group_name = ns("pos"),
                    orientation = "horizontal",
                    add_rank_list(
                      text = "PRIMARY POSITION(S)",
                      labels = positions()$primary,
                      input_id = ns("primary")
                    ),
                    add_rank_list(
                      text = "SECONDARY POSITION(S)",
                      labels = positions()$secondary,
                      input_id = ns("secondary")
                    ),
                    add_rank_list(
                      text = "Drag from here",
                      labels = positions()$remaining,
                      input_id = ns("unusedPositions")
                    )
                  )    
                )
              }
            )
          )
        )
        
      })
      
      #### OUTPUT SERVER ####
      playerInfo$server(id = "info", updated, playerData)
      playerHistory$server(id = "history", updated, playerData)
      
      output$playerName <- shiny$renderUI({
        data <- playerData()
        
        shiny$tagList(
          shiny$h2(paste(data$name, paste0("(", data$class, ")"), sep = " ")),
          shiny$h3(paste0("@", data$username))
        )
      }) |> 
        shiny$bindCache(updated())
      
      output$clubLogo <- shiny$renderUI({
        data <- playerData()
        
        shiny$img(
          src = sprintf("static/logo/%s.png", data$team),
          style = "height: 100px;",
          alt = data$team,
          title = data$team
        )
      }) |>
        shiny$bindCache(updated())
      
      output$playerInfo <- shiny$renderUI({
        data <- playerData()
        
        value <-
          data |>
          dplyr$select(
            dplyr$contains("pos_")
          ) |>
          pivot_longer(
            dplyr$everything()
          ) |>
          dplyr$mutate(
            name = str_remove(name, pattern = "pos_") |>
              str_to_upper()
          )
        
        shiny$tagList(
          bslib$layout_columns(
            col_widths = c(6, 6),
            shiny$tagList(
              shiny$h4(paste("TPE: ", data$tpe)),
              shiny$h4(paste("Banked TPE: ", data$tpebank)),
              shiny$h4(paste("Player Status: "), data$playerStatus, class = data$playerStatus),
              shiny$h4(paste("User Status: "), data$userStatus, class = data$userStatus),
              shiny$h5(paste("Nationality:"), data$nationality),
              shiny$h5(paste("Render: "), data$render)
            ),
            shiny$tagList(
              shiny$h4("Traits"),
              data$traits |>
                str_split(pattern = constant$traitSep) |>
                unlist() |>
                paste(collapse = "<br>") |>
                shiny$HTML(),
              shiny$br(),
              shiny$h4("Primary Position(s)"),
              value |>
                dplyr$filter(value == 20) |>
                dplyr$select(name) |>
                unlist() |>
                paste(collapse = ", ") |>
                shiny$HTML(),
              shiny$h4("Secondary Position(s)"),
              value |>
                dplyr$filter(value < 20, value >= 10) |>
                dplyr$select(name) |>
                unlist() |>
                paste(collapse = ", ") |>
                shiny$HTML()
            )
          )
        )
      }) |> 
        shiny$bindCache(updated())
      
      output$cost <- shiny$renderText({
        totalCost()
      })
      
      #### REACTIVES ####
      playerData <- shiny$reactive({
        getActivePlayer(auth$uid) |> 
          getPlayer()
      }) |> 
        shiny$bindEvent(updated())
      
      traitSum <- shiny$reactiveVal({0})
      positionSum <- shiny$reactiveVal({0})
      footSum <- shiny$reactiveVal({0})
      
      positions <- shiny$reactive({
        currentPositions <- 
          playerData() |> 
            dplyr$select(pos_gk:pos_st) |> 
            pivot_longer(cols = dplyr$everything()) |> 
            dplyr$mutate(name = name |> 
                           str_remove("pos_") |> 
                           str_to_upper())
        
        primary <- 
          constant$positions[
            names(constant$positions) %in% 
              (currentPositions |> 
                 dplyr$filter(value == 20) |>
                 dplyr$select(name) |> 
                 unlist()
               )
            ]
        
        secondary <- 
          constant$positions[
            names(constant$positions) %in% 
              (currentPositions |> 
                 dplyr$filter(value < 20 & value > 5) |>
                 dplyr$select(name) |> 
                 unlist()
              )
          ]
        
        remaining <- 
          constant$positions[
            !(
              constant$positions %in% c(primary, secondary)
            )
          ]
        
        list(
          primary = primary,
          secondary = secondary,
          remaining = remaining
        )
      })
      
      totalCost <- shiny$reactive({
        sum(
          input$individualTraining * 425000,
          traitSum(),
          positionSum(),
          footSum()
        )
      })
      
      currentTraits <- shiny$reactive({
        playerData()$traits |> 
          str_split(constant$traitSep) |> 
          unlist()
      })
      
      nrTraits <- shiny$reactiveVal({
        currentTraits() |> 
          length()
      })
      
      #### OBSERVERS ####
      ## Disables clashing traits
      shiny$observe({
        selected <- input$traits
        
        disable_list <- character()
        if ("Cuts Inside From Both Wings" %in% selected) {
          disable_list <- c(disable_list, "Avoids Using Weaker Foot")
        }
        if ("Knocks Ball Past Opponent" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        if ("Runs With Ball Rarely" %in% selected) {
          disable_list <- c(disable_list, "Knocks Ball Past Opponent", "Runs With Ball Often", "Runs With Ball Down Left", "Runs With Ball Down Right", "Runs With Ball Through Centre")
        }
        if ("Runs With Ball Often" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        if ("Runs With Ball Down Left" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Right", "Runs With Ball Through Centre", "Runs With Ball Rarely")
        }
        if ("Runs With Ball Down Right" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Left", "Runs With Ball Through Centre", "Runs With Ball Rarely")
        }
        if ("Runs With Ball Through Centre" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Left", "Runs With Ball Down Right", "Runs With Ball Rarely")
        }
        if ("Arrives Late In Opponent's Area" %in% selected) {
          disable_list <- c(disable_list, "Stays Back At All Times", "Gets Into Opposition Area")
        }
        if ("Gets Into Opposition Area" %in% selected) {
          disable_list <- c(disable_list, "Arrives Late In Opponent's Area", "Hugs Line", "Stays Back At All Times")
        }
        if ("Comes Deep To Get Ball" %in% selected) {
          disable_list <- c(disable_list, "Gets Forward Whenever Possible", "Likes To Try To Beat Offside Trap")
        }
        if ("Gets Forward Whenever Possible" %in% selected) {
          disable_list <- c(disable_list, "Comes Deep To Get Ball", "Stays Back At All Times", "Hugs Line")
        }
        if ("Likes To Try To Beat Offside Trap" %in% selected) {
          disable_list <- c(disable_list, "Comes Deep To Get Ball", "Does Not Move Into Channels", "Plays With Back To Goal")
        }
        if ("Hugs Line" %in% selected) {
          disable_list <- c(disable_list, "Gets Into Opposition Area")
        }
        if ("Plays With Back To Goal" %in% selected) {
          disable_list <- c(disable_list, "Likes To Try To Beat Offside Trap")
        }
        if ("Does Not Move Into Channels" %in% selected) {
          disable_list <- c(disable_list, "Moves Into Channels", "Likes To Try To Beat Offside Trap")
        }
        if ("Moves Into Channels" %in% selected) {
          disable_list <- c(disable_list, "Does Not Move Into Channels", "Stays Back At All Times")
        }
        if ("Stays Back At All Times" %in% selected) {
          disable_list <- c(disable_list, "Arrives Late In Opponent's Area", "Gets Forward Whenever Possible", "Gets Into Opposition Area", "Moves Into Channels")
        }
        if ("Likes To Switch Ball To Other Flank" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes")
        }
        if ("Looks For Pass Rather Than Attempting To Score" %in% selected) {
          disable_list <- c(disable_list, "Tries First Time Shots")
        }
        if ("Plays No Through Balls" %in% selected) {
          disable_list <- c(disable_list, "Tries Killer Balls Often")
        }
        if ("Plays Short Simple Passes" %in% selected) {
          disable_list <- c(disable_list, "Likes To Switch Ball To Other Flank", "Tries Killer Balls Often", "Tries Long Range Passes")
        }
        if ("Tries Killer Balls Often" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes", "Plays No Through Balls")
        }
        if ("Tries Long Range Passes" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes")
        }
        if ("Hits Free Kicks With Power" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Places Shots" %in% selected) {
          disable_list <- c(disable_list, "Shoots With Power")
        }
        if ("Refrains From Taking Long Shots" %in% selected) {
          disable_list <- c(disable_list, "Hits Free Kicks With Power", "Tries Long Range Free Kicks")
        }
        if ("Shoots From Distance" %in% selected) {
          disable_list <- c(disable_list, "Looks For Pass Rather Than Attempting To Score", "Refrains From Taking Long Shots")
        }
        if ("Shoots With Power" %in% selected) {
          disable_list <- c(disable_list, "Places Shots")
        }
        if ("Tries First Time Shots" %in% selected) {
          disable_list <- c(disable_list, "Looks For Pass Rather Than Attempting To Score")
        }
        if ("Tries Long Range Free Kicks" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Shoots From Distance" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Dives Into Tackles" %in% selected) {
          disable_list <- c(disable_list, "Does Not Dive Into Tackles")
        }
        if ("Does Not Dive Into Tackles" %in% selected) {
          disable_list <- c(disable_list, "Dives Into Tackles")
        }
        if ("Avoids Using Weaker Foot" %in% selected) {
          disable_list <- c(disable_list, "Cuts Inside From Both Wings")
        }
        if ("Tries To Play Way Out Of Trouble" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        
        # if(length(disable_list) == 0){
        #   disable_list <- "none"
        # }
        
        ## If you hit 7 traits none are purchasable 
        if((nrTraits() >= 7) | (input$traits |> length() >= 7)){
          allTraits <- 
            constant$traits |> unlist(use.names = FALSE)
          
          disable_list <- c(disable_list, allTraits[!(allTraits %in% input$traits)])
        }
        
        session$sendCustomMessage("disableCheckbox", disable_list |> unique())
      }) |>  
        shiny$bindEvent(
          input$traits,
          ignoreInit = FALSE,
          ignoreNULL = FALSE
        )
      
      ## Calculates sum of trait purchase
      shiny$observe({
        removed <- sum(!(currentTraits() %in% input$traits))
        added <- sum(!(input$traits %in% currentTraits()))
        
        traitSum(removed * 500000 + added * 3000000)
      }) |> 
        shiny$bindEvent(
          input$traits,
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      
      ## Calculates sum of positions purchase
      shiny$observe({
        summary <- 
          dplyr$tibble(
            name = c(input$primary, input$secondary, input$unusedPositions),
            value = 
              c(
                rep(20, times = length(input$primary)), 
                rep(15, times = length(input$secondary)), 
                rep(0, times = length(input$unusedPositions))
              )
          ) |> 
          dplyr$left_join(
            playerData() |> 
              dplyr$select(pos_gk:pos_st) |> 
              pivot_longer(cols = dplyr$everything()) |> 
              dplyr$mutate(name = name |> 
                             str_remove("pos_") |> 
                             str_to_upper()),
            by = "name",
            suffix = c(".new", ".old")
          ) |> 
          dplyr$mutate(
            change = dplyr$case_when(
              value.old == 0 & value.new == 20 ~ "Addition + Upgrade",
              value.old == 0 & value.new == 15 ~ "Addition",
              value.old >= 5 & value.old <  20 & value.new == 20 ~ "Upgrade",
              value.old == 20& value.new == 15 ~ "Downgrade",
              value.old == 20& value.new == 0  ~ "Downgrade + Remove",
              value.old >= 5 & value.old <  20 & value.new == 0  ~ "Remove",
              TRUE ~ "No Change"
            )
          ) |> 
          dplyr$group_by(change) |> 
          dplyr$summarize(n = dplyr$n()) |> 
          dplyr$mutate(
            cost = dplyr$case_when(
              change == "Addition + Upgrade" ~ n*20000000,
              change == "Upgrade" ~ n*7500000,
              change == "Addition" ~ n*12500000,
              change == "Downgrade + Remove" ~ -n*20000000,
              change == "Downgrade" ~ -n*7500000,
              change == "Remove" ~ -n*12500000,
              TRUE ~ 0
            )
          ) |> 
          dplyr$ungroup()
        
        ## Calculates number of swaps
        swaps <- 
          summary |>
          dplyr$summarize(
            Addition = dplyr$if_else(str_detect(change, "^Addition$"), n, 0),
            Remove = dplyr$if_else(str_detect(change, "^Remove$"), n, 0),
            Upgrade = dplyr$if_else(str_detect(change, "^Upgrade$"), n, 0),
            Downgrade = dplyr$if_else(str_detect(change, "^Downgrade$"), n, 0),
            AdditionPlus = dplyr$if_else(str_detect(change, "Addition +"), n, 0),
            RemovePlus = dplyr$if_else(str_detect(change, "Downgrade +"), n, 0)
          ) |> 
          dplyr$summarize(
            Addition = sum(Addition),
            Remove = sum(Remove),
            Upgrade = sum(Upgrade),
            Downgrade = sum(Downgrade),
            AdditionPlus = sum(AdditionPlus),
            RemovePlus = sum(RemovePlus)
          ) |> 
          dplyr$summarize(
            swaps = floor((Addition + Remove)/2) + floor((Upgrade + Downgrade)/2) + floor((AdditionPlus + RemovePlus)/2)
          )
        
        summary <-
          summary |> 
          dplyr$mutate(
            cost = 
              dplyr$if_else(
                change == "No Change", 
                cost + dplyr$if_else(swaps$swaps > 0, swaps$swaps, 0) * 5000000, 
                cost
              )
          )
        
        ## Only calculate cost if all 13 positions are accounted for
        if( summary$n |> sum() == 13) {
          positionSum({sum(summary$cost)})
        }
        
        if( summary$cost |> sum() < 0) {
          showToast(
            .options = constant$myToastOptions,
            "error",
            "You cannot only remove positions from your build, please adjust your positional purchase."
          )
          
          shiny$updateActionButton(
            inputId = "verifyPurchase", 
            label = "You cannot only remove positions!"
          )
          
          disable(id = "verifyPurchase")
        } else {
          shiny$updateActionButton(
            inputId = "verifyPurchase", 
            label = "Verify purchase"
          )
          
          enable(id = "verifyPurchase")
        }
      }) |> 
        shiny$bindEvent(input$pos, ignoreInit = TRUE)
    }
  })
}
