box::use(
  bslib,
  dplyr,
  purrr[map],
  rlang[`!!!`],
  shiny,
  shinyFeedback[
    feedback, 
    feedbackDanger, 
    feedbackWarning, 
    hideFeedback, 
    showToast
  ],
  shinyjs,
  shiny.router[change_page],
  sortable[add_rank_list, bucket_list],
  stringr[str_remove_all],
  tidyr[replace_na],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[
    checkApprovingPlayer,
    checkDuplicatedNames,
    eligibleRedist, 
    eligibleReroll, 
    hasActivePlayer,
    submitBuild,
    verifyBuild
    ],
  app/logic/ui/tags[flexCol, flexRow, numericStepper],
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
      showToast(
        .options = constant$sslToastOptions,
        "error",
        "You do not have access to this page, please log in!"
      )
      
      change_page("")
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)){
      showToast(
        .options = constant$sslToastOptions,
        "error",
        "You do not have access to this page."
      )
      
      change_page("")
    } else if (hasActivePlayer(auth$uid)) {
      output$ui <- shiny$renderUI({
        "YOU HAVE ALREADY CREATED A PLAYER."
      })
    } else if (checkApprovingPlayer(auth$uid)) {
      output$ui <- shiny$renderUI({
        "YOUR PLAYER IS AWAITING APPROVAL."
      })
    } else {
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        shiny$tagList(
          shiny$h3("Instructions"),
          shiny$p("Please fill out all the given areas with information about
                  your player and their build. Required fields are marked with *."),
          shiny$p(
            paste("To help you in this process, we have created a", 
                  shiny$a("Player Compendium", 
                          href = "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit?usp=sharing",
                          target = "_blank"),
                  "that includes detailed descriptions of the 
                  player attributes, which are important for 
                  different player positions and roles, as well 
                  as descriptions of player traits."
                  ) |> 
              shiny$HTML()
          ),
          shiny$br(),
          shiny$h4("Player Information", align = "center"),
          bslib$layout_column_wrap(
            width = 1/3,
            shiny$textInput(ns("firstName"), "Enter a first name:", placeholder = "First Name"),
            shiny$textInput(ns("lastName"), "*Enter a last name:", placeholder = "Last Name"),
            shiny$selectInput(
              ns("pronouns"), 
              tippy(
                "*Select your player's pronouns",
                "This is used by commentators to correctly gender your player. Multiple selections are allowed",
                theme = "ssl"
              ),
              choices = c("He/Him", "She/Her", "They/Them"),
              multiple = TRUE
            ),
            shiny$textInput(
              ns("birthplace"), 
              tippy("Enter a place of birth:", "Only thematic", theme = "ssl"), 
              placeholder = "City"
            ),
            shiny$selectInput(
              ns("nationality"), 
              tippy("*Select a nationality:", "Defines your player's international region affiliation, see the WSFC Tracker for details and current region sizes", theme = "ssl"),
              choices = constant$sslNations
            ),
            shiny$numericInput(
              ns("height"), 
              tippy("Enter height (inches):", "Only cosmetic", theme = "ssl"), 
              value = 62, 
              min = 55, 
              max = 90
            ),
            shiny$numericInput(
              ns("weight"), 
              tippy("Enter weight (pounds):", "Only cosmetic", theme = "ssl"), 
              value = 180, 
              min = 100, 
              max = 350, 
              step = 5
            ),
            shiny$selectInput(ns("footedness"), "*Select preferred foot:", choices = c("", "Left", "Right")),
            shiny$textInput(
              ns("render"), 
              tippy("Select a player likeness:", 
                    "A player render is a person or thing you want your player to look like for graphics used within the league.",
                    theme = "ssl"), 
              placeholder = "ex. Lionel Messi"
            ),
            "",
            shiny$radioButtons(
              ns("playerType"), 
              "Outfield or Goalkeeper", 
              choices = c("Outfield", "Goalkeeper"), 
              selected = "Outfield", 
              inline = TRUE
            ),
            ""
          ),
          shiny$h4("FM Cosmetics", align = "center"),
          bslib$layout_column_wrap(
            width = 1/3,
            shiny$selectInput(
              ns("hairColor"), 
              tippy("*Select hair color:", "Lower values are lighter shades", theme = "ssl"), 
              choices = 
                c(
                  "",
                  "Light Brown 1" = "LBR1",
                  "Light Brown 2" = "LBR2",
                  "Light Brown 3" = "LBR3",
                  "Dark Brown 1" = "DBR1",
                  "Dark Brown 2" = "DBR2",
                  "Dark Brown 3" = "DBR3",
                  "Blond(e) 1" = "BLN1",
                  "Blond(e) 2" = "BLN2",
                  "Blond(e) 3" = "BLN3",
                  "Black 1" = "BLK1",
                  "Black 2" = "BLK2",
                  "Black 3" = "BLK3",
                  "Red 1" = "RED1",
                  "Red 2" = "RED2",
                  "Red 3" = "RED3"
                )
            ),
            shiny$selectInput(ns("hairLength"), "*Select hair length:", choices = c("", "Bald", "Buzzcut", "Short", "Medium", "Long")),
            shiny$selectInput(
              ns("skinColor"), 
              "*Select skin tone:", 
              choices = 
                c(
                  "",
                  "Skin Tone 1 (lightest)" = "1",
                  "Skin Tone 2" = "2",
                  "Skin Tone 3" = "3",
                  "Skin Tone 4" = "4",
                  "Skin Tone 5" = "5",
                  "Skin Tone 6" = "6",
                  "Skin Tone 7" = "7",
                  "Skin Tone 8" = "8",
                  "Skin Tone 9" = "9",
                  "Skin Tone 10" = "10",
                  "Skin Tone 11" = "11",
                  "Skin Tone 12" = "12",
                  "Skin Tone 13" = "13",
                  "Skin Tone 14" = "14",
                  "Skin Tone 15" = "15",
                  "Skin Tone 16" = "16",
                  "Skin Tone 17" = "17",
                  "Skin Tone 18" = "18",
                  "Skin Tone 19" = "19",
                  "Skin Tone 20 (darkest)" = "20"
                )
            )
          ),
          shiny$h4("Player Attributes", align = "center"),
          shiny$uiOutput(ns("roleSelector")),
          shiny$uiOutput(ns("attributes")),
          shiny$uiOutput(ns("outfieldExtras")),
          bslib$layout_column_wrap(
            width = 1/2,
            shiny$textOutput(ns("tpeRemaining")),
            shiny$actionButton(ns("submit"), label = "Submit Player")
          ) |> 
            shiny$div(class = "frozen-bottom"),
          shiny$div(style = "min-height:100px;")
        )
      })
      
      #### OUTPUT SERVER ####
      
      output$attributes <- shiny$renderUI({
        map(
          .x = attributeGroups(),
          .f = function(chosengroup) {
            output[[chosengroup]] <- shiny$renderUI({
              l1 <- 
                lapply(
                  attributes()$una |> 
                    dplyr$filter(group == chosengroup) |> 
                    dplyr$select(attribute) |> 
                    unlist(),
                  FUN = function(currentAtt) {
                    attID <- currentAtt |> str_remove_all(" ")
                    
                    shiny$tagList(
                      numericStepper(
                        inputId = ns(attID),
                        value = 5,
                        disabled = TRUE
                      )
                    ) |> 
                      shinyjs$hidden()
                  }
                )
              
              
              l2 <- 
                lapply(
                  attributes()$ava |> 
                    dplyr$filter(group == chosengroup) |> 
                    dplyr$select(attribute) |> 
                    unlist(),
                  FUN = function(currentAtt) {
                    attID <- currentAtt |> str_remove_all(" ")
                    
                    if(currentAtt %in% c("Natural Fitness", "Stamina")){
                      shiny$div(
                        class = "player-attribute-editor",
                        shiny$div(
                          class = "attribute-editor-header",
                          shiny$tagList(
                            shiny$div(
                              shiny$tagList(
                                shiny$span(style = "font-weight: 700;", 
                                           tippy(
                                             currentAtt,
                                             constant$attributes |> 
                                               dplyr$filter(attribute == currentAtt) |> 
                                               dplyr$select(explanation),
                                             theme = "ssl"
                                           )
                                )
                              )
                            ),
                            numericStepper(
                              inputId = ns(attID),
                              value = 20,
                              disabled = TRUE
                            )
                          )
                        )
                      )
                    } else {
                      shiny$div(
                        class = "player-attribute-editor",
                        shiny$div(
                          class = "attribute-editor-header",
                          shiny$tagList(
                            shiny$div(
                              shiny$tagList(
                                if (constant$roleAttributes[[input$selectedRole]][[attID]] == 1) {
                                  shiny$icon("circle-exclamation", style = "color: var(--important);")
                                } else if (constant$roleAttributes[[input$selectedRole]][[attID]] == 2) {
                                  shiny$icon("circle-exclamation", style = "color: var(--key);")
                                },
                                shiny$span(style = "font-weight: 700;", 
                                           tippy(
                                             currentAtt,
                                             constant$attributes |> 
                                               dplyr$filter(attribute == currentAtt) |> 
                                               dplyr$select(explanation),
                                             theme = "ssl"
                                           )
                                )
                              )
                            ),
                            numericStepper(
                              inputId = ns(attID),
                              value = 5,
                              min = 5,
                              max = 20
                            )
                          )
                        ),
                        shiny$uiOutput(ns(paste0("cost", attID)))
                      )
                    }
                  }
                )
                
                shiny$tagList(
                  l1, l2
                )
            })
          }
        )
        
        map(
          .x = constant$attributes$attribute,
          .f = function(attribute){
            output[[paste0("cost", attribute |> str_remove_all(" "))]] <- shiny$renderUI({
              curValue <- input[[attribute |> str_remove_all(" ")]]
              
              if(attribute %in% c("Natural Fitness", "Stamina")){
                
              } else {
                shiny$div(
                  class = "attribute-editor-footer",
                  shiny$tagList(
                    shiny$span(
                      "Total: ",
                      constant$tpeCost |> 
                        dplyr$filter(value == curValue) |> 
                        dplyr$select(cumCost) |> unlist() |> 
                        replace_na(0)
                    ),
                    shiny$span(
                      "Next: ",
                      next_cost <- constant$tpeCost |> 
                        dplyr$filter(value == curValue + 1) |> 
                        dplyr$select(sinCost) |> unlist(),
                      if (length(next_cost) == 0 || is.na(next_cost)) "--"
                    )
                  )
                )
              }
            })
          }
        )
        
        uiList <- 
          map(
            .x = attributeGroups(),
            .f = function(chosengroup){
              shiny$tagList(
                shiny$h5(chosengroup),
                shiny$uiOutput(ns(chosengroup))  
              )
            }
          ) 
        
        bslib$layout_column_wrap(
          width = 1 / length(uiList),
          !!!unname(uiList)
        )
        
      })
      
      output$outfieldExtras <- shiny$renderUI({
        if(input$playerType == "Outfield") {
          shiny$tagList(
            shiny$tags$script(
              paste0(
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
                });
              "
              ) |> 
                shiny$HTML()
            ),
            shiny$h4("Player Traits and Positions", align = "center"),
            bucket_list(
              header = "An outfield player may select one primary position and two secondary positions.",
              group_name = ns("pos"),
              orientation = "horizontal",
              add_rank_list(
                text = "Select ONE primary position:",
                labels = NULL,
                input_id = ns("primary")
              ),
              add_rank_list(
                text = "Select TWO secondary positions:",
                labels = NULL,
                input_id = ns("secondary")
              ),
              add_rank_list(
                text = "Drag from here",
                labels = constant$positions,
                input_id = ns("unusedPositions")
              )
            ),
            shiny$checkboxGroupInput(
              ns("traits"), 
              "Select two (2) traits:", 
              choices = constant$traits |> 
                unlist(use.names = FALSE)
            ) |> 
              shiny$div(class = "multicol")
          )
        }
      })
      
      output$tpeRemaining <- shiny$renderText({
        paste("TPE Remaining: ", bankedTPE())
      })
      
      output$roleSelector <- shiny$renderUI({
        shiny$tagList(
          paste("Football Manager uses <i>roles</i> and <i>duties</i> to control what your player will do within a
        set tactic. There exists many different roles with different importances given to specific
        attributes. Selecting a role and duty you want to build towards in the list below will highlight the <span
        class='keyAttribute'>very important</span> and
        <span class='importantAttribute'>important</span>          attributes.") |> shiny$HTML(),
          shiny$br(),
          shiny$selectInput(
            ns("selectedRole"), 
            label = tippy("Select a player role", 
                          tooltip = "Please note, the role you play will be determined by your Manager. If you want to play a specific role, make sure to speak with your Manager.",
                          theme = "ssl"), 
            choices = names(constant$roleAttributes))
        )
      })
  
      #### REACTIVES ####
      attributeGroups <- shiny$reactive({
        constant$attributes |> 
          dplyr$filter(
            if (input$playerType == "Goalkeeper"){
              (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE") | (group %in% c("Physical", "Mental"))
            } else {
              group %in% c("Physical", "Mental", "Technical")
            }
          ) |> 
          dplyr$select(group) |> 
          unique() |> 
          dplyr$arrange() |> 
          unlist()
      }) |> 
        shiny$bindEvent(input$playerType)
      
      attributes <- shiny$reactive({
        available <- constant$attributes |> 
          dplyr$filter(
            if (input$playerType == "Goalkeeper"){
              (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE") | (group %in% c("Physical", "Mental"))
            } else {
              group %in% c("Physical", "Mental", "Technical")
            }
          ) 
        
        unavailable <- 
          constant$attributes |> 
          dplyr$filter(
            !(attribute %in% available$attribute)
          )
        
        list(ava = available, una = unavailable)
      }) |> 
        shiny$bindEvent(input$playerType)
      
      bankedTPE <- shiny$reactive({
        appliedTPE <- 
          map(
            .x = constant$attributes$attribute |> str_remove_all(" "),
            .f = function(currentAtt){
              curValue <- input[[currentAtt]]
              
              if(curValue |> is.null()){
                0
              } else {
                constant$tpeCost |> 
                  dplyr$filter(value == curValue) |> 
                  dplyr$select(cumCost) |> 
                  unlist()  
              }
              
            }
          ) |> 
          unlist() |> 
          sum()
        
        constant$startingTPE - appliedTPE + 2*(constant$tpeCost$cumCost |> max())
      }) |> 
        shiny$bindEvent(
          lapply(
            X = constant$attributes$attribute |> str_remove_all(" "),
            FUN = function(x){
              input[[x]]
            }
          ),
          ignoreInit = TRUE
        )
      
      #### OBSERVERS ####
      ## Fixes inputs outside of allowed ranges
      lapply(
        X = constant$attributes$attribute |> str_remove_all(" "),
        FUN = function(att){
          shiny$observe({
            currentVal <- input[[att]]
            
            shiny$req(currentVal)
            
            # Determine the dynamic min and max values.
            minVal <- 5
            maxVal <- 20
            
            # Constrain the input value within dynamic boundaries.
            adjustedVal <- min(max(currentVal, minVal), maxVal)
            # Update the numeric input only if the current value is out of bounds.
            if (currentVal != adjustedVal) {
              shiny$updateNumericInput(
                session,
                inputId = att,
                value = adjustedVal
              )
            }
          }) |> 
            shiny$bindEvent(
              input[[att]],
              ignoreInit = TRUE
            )
        }
      )
      
      lapply(
        X = c("weight", "height"),
        FUN = function(att){
          shiny$observe({
            currentVal <- input[[att]]
            
            shiny$req(currentVal)
            
            # Determine the dynamic min and max values.
            minVal <- dplyr$if_else(att == "weight", 100, 55)
            maxVal <- dplyr$if_else(att == "weight", 350, 90)
            
            # Constrain the input value within dynamic boundaries.
            adjustedVal <- min(max(currentVal, minVal), maxVal)
            
            # Update the numeric input only if the current value is out of bounds.
            if (currentVal != adjustedVal) {
              shiny$updateNumericInput(
                session,
                inputId = att,
                value = adjustedVal
              )
              
              
              
              feedbackWarning(
                session = session,
                inputId = att, 
                show = TRUE, 
                paste("The player's", att, "exceeds the limits and has been changed to the min/max.")
              )
            } else {
              # hideFeedback(
              #   session = session,
              #   inputId = att
              # )
            }
          }) |> 
            shiny$bindEvent(
              input[[att]],
              ignoreInit = TRUE
            )
        }
      )
      
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
          disable_list <- c(disable_list, "Comes Deep To Get Ball", "Stays Back At All Times")
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
        
        session$sendCustomMessage(
          "disableCheckbox", 
          disable_list |> unique()
        )
      }) |>  
        shiny$bindEvent(
          input$traits,
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      
      ## Highlights different attributes based on selected role
      shiny$observe({
        lapply(
          X = constant$attributes$attribute |> str_remove_all(" "),
          FUN = function(att){
            if(constant$roleAttributes[[input$selectedRole]][[att]] == 1){
              feedback(
                session = session,
                show = TRUE,
                inputId = att,
                color = constant$importantColor,
                icon = shiny$icon("exclamation-sign", lib = "glyphicon")
              )
            } else if(constant$roleAttributes[[input$selectedRole]][[att]] == 2){
              feedback(
                session = session,
                show = TRUE,
                inputId = att,
                color = constant$keyColor,
                icon = shiny$icon("exclamation-sign", lib = "glyphicon")
              )
            } else {
              hideFeedback(
                session = session,
                inputId = att
              )
            }
          }
        )
      }) |> 
        shiny$bindEvent(input$selectedRole)
      
      ## Verifies the creation
      shiny$observe({
        # Checks required fields
        if(
          (sapply(
            c(input$lastName, input$nationality, input$footedness, 
              input$hairColor, input$hairLength, input$skinColor), 
            FUN = function(x) x == "", simplify = TRUE
          ) |> any()) | 
          is.null(input$pronouns) | 
          (
            input$playerType == "Outfield" & 
            (input$traits |> length() != 2 | 
             input$primary |> length() != 1 | 
             input$secondary |> length() != 2)
           )
        ) {
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "You have missed some required field. Please check the marked fields."
          )
          
          feedbackDanger("lastName", input$lastName == "", "Please enter a last name for your player. If you only want to use one name, please enter it here instead of as a first name.")
          feedbackDanger("nationality", input$nationality == "", "Please enter the nationality of your player.")
          feedbackDanger("footedness", input$footedness == "", "Please enter the footedness of your player.")
          feedbackDanger("hairColor", input$hairColor == "", "Please enter the hair color for your player.")
          feedbackDanger("hairLength", input$hairLength == "", "Please enter the hair length for your player.")
          feedbackDanger("skinColor", input$skinColor == "", "Please enter the skin tone for your player.")
          feedbackDanger("pronouns", is.null(input$pronouns), "Please enter at least one pronoun for your player.")
          
          if(input$traits |> length() != 2){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              "Please select two (2) traits."
            )
          }
          
          if(input$primary |> length() != 1){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              "You can select one (1) primary position."
            )
          }
          
          if(input$secondary |> length() != 2){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              "You can select two (2) secondary positions."
            )
          }
        } else if (checkDuplicatedNames(input$first, input$last)){
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "Another player in the league's history have used this name. 
            Please change it to something else."
          )
        } else if( attributes()$ava$attribute |> str_remove_all(" ") |> 
                   sapply(X = _, FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE) |> 
                   unlist() |> 
                   any()
        ) {
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "One or more of your attributes are lower than 5 or higher than 20, 
            which exceeds the range of attributes we allow."
          )
        } else if (bankedTPE() < 0){
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "You have spent too much TPE. Please adjust and resubmit."
          )
        } else if (bankedTPE() > 50) {
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "Please allocate as much of the TPE you are given as possible. 
            If you need help with your build, 
            reach out to an Academy coach on Discord."
          )
        } else {
          # Finally if all checks pass, produce a verification modal
          verifyBuild(input, bankedTPE, session)
        }
      }) |> 
        shiny$bindEvent(input$submit)
      
      
      shiny$observe({
        shiny$removeModal()
        
        tryCatch({
          submitBuild(input, bankedTPE, auth)
          
          updated(updated() + 1)
          
          change_page("/")
          
          showToast(
            .options = constant$sslToastOptions,
            "success", 
            "Your player has been submitted for approval. 
          You will be notified via the forum or Discord by a 
          member of the BoD when the approval has been completed 
          or if there are any issues."
          )
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "Something has gone wrong, contact Canadice."
          )
          
          message("Create a player error: ", e)
        })
      }) |> 
        shiny$bindEvent(input$confirmBuild)
    }
  })
}
