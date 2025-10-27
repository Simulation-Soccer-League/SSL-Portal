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
    getOrganizations,
    getPlayer,
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
            shiny$h3("Edit Player")
          ),
          bslib$card_body(
            shiny$p(
              "This page is used to edit player information, such as updating the player render,
              upgrading positional xp from the Career PT#4 reward."
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
                shiny$h4("Player Information"),
                shiny$textInput(
                  ns("render"),
                  "Change render",
                  value = ""
                ),
                bslib$layout_column_wrap(
                  width = 1 / 2,
                  shiny$selectInput(
                    ns("left"), 
                    "Change left foot",
                    choices = c(10, 15, 20)
                  ),
                  shiny$selectInput(
                    ns("right"), 
                    "Change right foot",
                    choices = c(10, 15, 20)
                  )
                )
              )
            ),
            shiny$h4("Positions and traits"),
            shiny$uiOutput(ns("positions")),
            shiny$checkboxGroupInput(
              ns("traits"), 
              "Change traits", 
              choices = constant$traits |> unlist(use.names = FALSE), 
              selected = NULL
            ) |>  
              shiny$div(class = "multicol"),
            shiny$tags$script(
              paste(
                "Shiny.addCustomMessageHandler('disableCheckbox', function(checkboxId) {
                      if (typeof checkboxId === 'string') {
                        checkboxId = [checkboxId]; // Convert single string to array
                      }
                      var checkboxes = document.getElementsByName('", ns("traits"), "');
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
                    });",
                sep = ""
              ) |> 
                shiny$HTML()
            )
          )
        )
      )
    })
    
    #### REACTIVES ####
    playerData <- shiny$reactive({
      shiny$req(input$selectedPlayer)
      
      pid <- input$selectedPlayer
      
      getPlayer(pid)
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
      getOrganizations() |> 
        dplyr$select(id = ID, name = organization, abbr = abbreviation) |> 
        dplyr$filter(!is.na(name)) |> 
        dplyr$distinct()
    })
    
    positions <- shiny$reactive({
      currentPositions <- 
        playerData() |> 
        dplyr$select(pos_gk:pos_st) |> 
        pivot_longer(cols = dplyr$everything()) |> 
        dplyr$mutate(name = name |> 
                       str_remove("pos_") |> 
                       str_to_upper())
      
      primary <- 
        constant$positionsGK[
          names(constant$positionsGK) %in% 
            (currentPositions |> 
               dplyr$filter(value == 20) |>
               dplyr$select(name) |> 
               unlist()
            )
        ]
      
      secondary <- 
        constant$positionsGK[
          names(constant$positionsGK) %in% 
            (currentPositions |> 
               dplyr$filter(value < 20 & value > 5) |>
               dplyr$select(name) |> 
               unlist()
            )
        ]
      
      remaining <- 
        constant$positionsGK[
          !(
            constant$positionsGK %in% c(primary, secondary)
          )
        ]
      
      list(
        primary = primary,
        secondary = secondary,
        remaining = remaining
      )
    }) |> 
      shiny$bindEvent(
        playerData()
      )
    
    currentTraits <- shiny$reactive({
      playerData()$traits |> 
        str_split(constant$traitSep) |> 
        unlist()
    }) |> 
      shiny$bindEvent(
        playerData()
      )
    
    nrTraits <- shiny$reactive({
      currentTraits() |> 
        length()
    }) |> 
      shiny$bindEvent(
        currentTraits()
      )
    
    #### OUTPUT SERVER ####
    output$positions <- shiny$renderUI({
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
    })
    
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
        inputId = "render",
        value = playerData()$render
      )
      
      shiny$updateSelectInput(
        session,
        inputId = "left",
        selected = playerData()$`left foot`
      )
      
      shiny$updateSelectInput(
        session,
        inputId = "right",
        selected = playerData()$`right foot`
      )
      
      shiny$updateCheckboxGroupInput(
        session,
        inputId = "traits",
        selected = currentTraits()
      )
      
    }) |> 
      shiny$bindEvent(playerData())
    
    ## Disables clashing traits
    shiny$observe({
      selected <- input$traits
      
      disableList <- character()
      if ("Cuts Inside From Both Wings" %in% selected) {
        disableList <- c(disableList, "Avoids Using Weaker Foot")
      }
      if ("Knocks Ball Past Opponent" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Rarely")
      }
      if ("Runs With Ball Rarely" %in% selected) {
        disableList <- c(disableList, "Knocks Ball Past Opponent", 
                         "Runs With Ball Often", "Runs With Ball Down Left", 
                         "Runs With Ball Down Right", "Runs With Ball Through Centre")
      }
      if ("Runs With Ball Often" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Rarely")
      }
      if ("Runs With Ball Down Left" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Down Right", 
                         "Runs With Ball Through Centre", "Runs With Ball Rarely")
      }
      if ("Runs With Ball Down Right" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Down Left", 
                         "Runs With Ball Through Centre", "Runs With Ball Rarely")
      }
      if ("Runs With Ball Through Centre" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Down Left", 
                         "Runs With Ball Down Right", "Runs With Ball Rarely")
      }
      if ("Arrives Late In Opponent's Area" %in% selected) {
        disableList <- c(disableList, "Stays Back At All Times", 
                         "Gets Into Opposition Area")
      }
      if ("Gets Into Opposition Area" %in% selected) {
        disableList <- c(disableList, "Arrives Late In Opponent's Area", "Hugs Line", 
                         "Stays Back At All Times")
      }
      if ("Comes Deep To Get Ball" %in% selected) {
        disableList <- c(disableList, "Gets Forward Whenever Possible", 
                         "Likes To Try To Beat Offside Trap")
      }
      if ("Gets Forward Whenever Possible" %in% selected) {
        disableList <- c(disableList, "Comes Deep To Get Ball", "Stays Back At All Times")
      }
      if ("Likes To Try To Beat Offside Trap" %in% selected) {
        disableList <- c(disableList, "Comes Deep To Get Ball", "Does Not Move Into Channels", 
                         "Plays With Back To Goal")
      }
      if ("Hugs Line" %in% selected) {
        disableList <- c(disableList, "Gets Into Opposition Area")
      }
      if ("Plays With Back To Goal" %in% selected) {
        disableList <- c(disableList, "Likes To Try To Beat Offside Trap")
      }
      if ("Does Not Move Into Channels" %in% selected) {
        disableList <- c(disableList, "Moves Into Channels", "Likes To Try To Beat Offside Trap")
      }
      if ("Moves Into Channels" %in% selected) {
        disableList <- c(disableList, "Does Not Move Into Channels", "Stays Back At All Times")
      }
      if ("Stays Back At All Times" %in% selected) {
        disableList <- c(disableList, "Arrives Late In Opponent's Area", 
                         "Gets Forward Whenever Possible", "Gets Into Opposition Area", 
                         "Moves Into Channels")
      }
      if ("Likes To Switch Ball To Other Flank" %in% selected) {
        disableList <- c(disableList, "Plays Short Simple Passes")
      }
      if ("Looks For Pass Rather Than Attempting To Score" %in% selected) {
        disableList <- c(disableList, "Tries First Time Shots")
      }
      if ("Plays No Through Balls" %in% selected) {
        disableList <- c(disableList, "Tries Killer Balls Often")
      }
      if ("Plays Short Simple Passes" %in% selected) {
        disableList <- c(disableList, "Likes To Switch Ball To Other Flank", 
                         "Tries Killer Balls Often", "Tries Long Range Passes")
      }
      if ("Tries Killer Balls Often" %in% selected) {
        disableList <- c(disableList, "Plays Short Simple Passes", "Plays No Through Balls")
      }
      if ("Tries Long Range Passes" %in% selected) {
        disableList <- c(disableList, "Plays Short Simple Passes")
      }
      if ("Hits Free Kicks With Power" %in% selected) {
        disableList <- c(disableList, "Refrains From Taking Long Shots")
      }
      if ("Places Shots" %in% selected) {
        disableList <- c(disableList, "Shoots With Power")
      }
      if ("Refrains From Taking Long Shots" %in% selected) {
        disableList <- c(disableList, "Hits Free Kicks With Power", "Tries Long Range Free Kicks")
      }
      if ("Shoots From Distance" %in% selected) {
        disableList <- c(disableList, "Looks For Pass Rather Than Attempting To Score", 
                         "Refrains From Taking Long Shots")
      }
      if ("Shoots With Power" %in% selected) {
        disableList <- c(disableList, "Places Shots")
      }
      if ("Tries First Time Shots" %in% selected) {
        disableList <- c(disableList, "Looks For Pass Rather Than Attempting To Score")
      }
      if ("Tries Long Range Free Kicks" %in% selected) {
        disableList <- c(disableList, "Refrains From Taking Long Shots")
      }
      if ("Shoots From Distance" %in% selected) {
        disableList <- c(disableList, "Refrains From Taking Long Shots")
      }
      if ("Dives Into Tackles" %in% selected) {
        disableList <- c(disableList, "Does Not Dive Into Tackles")
      }
      if ("Does Not Dive Into Tackles" %in% selected) {
        disableList <- c(disableList, "Dives Into Tackles")
      }
      if ("Avoids Using Weaker Foot" %in% selected) {
        disableList <- c(disableList, "Cuts Inside From Both Wings")
      }
      if ("Tries To Play Way Out Of Trouble" %in% selected) {
        disableList <- c(disableList, "Runs With Ball Rarely")
      }
      
      ## If you hit 7 traits none are purchasable 
      if ((nrTraits() >= 7) | (input$traits |> length() >= 7)) {
        allTraits <- 
          constant$traits |> unlist(use.names = FALSE)
        
        disableList <- c(disableList, allTraits[!(allTraits %in% input$traits)])
      }
      
      session$sendCustomMessage("disableCheckbox", disableList |> unique())
    }) |>  
      shiny$bindEvent(
        input$traits,
        ignoreInit = FALSE,
        ignoreNULL = FALSE
      )
    
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
            "Something has gone wrong, contact Canadice."
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
