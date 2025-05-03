box::use(
  bslib,
  dplyr,
  purrr[map],
  rlang[`!!!`],
  shiny,
  shinyjs,
  shiny.router[change_page],
  stringr[str_remove_all],
  tidyr[replace_na],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[eligibleRedist, eligibleReroll, hasActivePlayer],
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
    } else if (hasActivePlayer(auth$uid)) {
      output$ui <- shiny$renderUI({
        "YOU ALREADY HAVE A CREATED PLAYER."
      })
    } else {
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        shiny$tagList(
          shiny$h3("Instructions"),
          shiny$p("Please fill out all the given areas with information about
                  your player and their build."),
          shiny$p(
            paste("To help you in this process, we have created a", 
                  shiny$a("Player Compendium", 
                          href = "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit?usp=sharing"),
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
            shiny$textInput(ns("lastName"), "Enter a last name:", placeholder = "Last Name"),
            shiny$textInput(
              ns("birthplace"), 
              tippy("Enter a place of birth:", "Only thematic", theme = "ssl"), 
              placeholder = "City"
            ),
            shiny$selectInput(
              ns("nationality"), 
              tippy("Select a nationality:", "Defines your player's international region affiliation, see the WSFC Tracker for details and current region sizes", theme = "ssl"),
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
            shiny$selectInput(ns("footedness"), "Select preferred foot:", choices = c("", "Left", "Right")),
            shiny$textInput(
              ns("render"), 
              tippy("Select a player likeness:", 
                    "A player render is a person or thing you want your player to look like for graphics used within the league.",
                    theme = "ssl"), 
              placeholder = "ex. Lionel Messi"
            ),
            shiny$radioButtons(
              ns("playerType"), 
              "Outfield or Goalkeeper", 
              choices = c("Outfield", "Goalkeeper"), 
              selected = "Outfield", 
              inline = TRUE
            )
          ),
          shiny$h4("FM Cosmetics", align = "center"),
          bslib$layout_column_wrap(
            width = 1/3,
            shiny$selectInput(
              ns("hairColor"), 
              tippy("Select hair color:", "Lower values are lighter shades", theme = "ssl"), 
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
            shiny$selectInput(ns("hairLength"), "Select hair length:", choices = c("", "Bald", "Buzzcut", "Short", "Medium", "Long")),
            shiny$selectInput(
              ns("skinColor"), 
              "Select skin tone:", 
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
          shiny$uiOutput(ns("attributes"))
        )
      })
      
      #### OUTPUT SERVER ####
      
      output$attributes <- shiny$renderUI({
        map(
          .x = attributeGroups(),
          .f = function(chosengroup){
            output[[chosengroup]] <- shiny$renderUI({
              temp <- 
                constant$attributes |> 
                dplyr$filter(
                  if (input$playerType == "Goalkeeper"){
                    group == chosengroup & keeper == TRUE
                  } else {
                    group == chosengroup
                  }
                ) |> 
                dplyr$select(attribute)
              
              lapply(
                temp$attribute,
                FUN = function(currentAtt){
                  if(currentAtt %in% c("Natural Fitness", "Stamina")){
                    shiny$tagList(
                      shiny$numericInput(
                        inputId = ns(currentAtt |> str_remove_all(" ")),
                        label = 
                          tippy(
                            currentAtt,
                            constant$attributes |> 
                              dplyr$filter(attribute == currentAtt) |> 
                              dplyr$select(explanation),
                            theme = "ssl"
                          ),
                        value = 20,
                        max = 20,
                        min = 20
                      ) |> 
                        shinyjs$disabled()
                    )
                  } else {
                    shiny$tagList(
                      shiny$numericInput(
                        inputId = ns(currentAtt |> str_remove_all(" ")),
                        label = 
                          tippy(
                            currentAtt,
                            constant$attributes |> 
                              dplyr$filter(attribute == currentAtt) |> 
                              dplyr$select(explanation),
                            theme = "ssl"
                          ),
                        value = 5,
                        min = 5,
                        max = 20
                      ),
                      shiny$uiOutput(ns(paste0("cost", currentAtt |> str_remove_all(" "))))
                    )
                  }
                }
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
                shiny$p(
                  paste0(
                    "Next: ", 
                    constant$tpeCost |> 
                      dplyr$filter(value == curValue + 1) |> 
                      dplyr$select(sinCost) |> unlist() |> 
                      replace_na(0),
                    " Total Cost: ",
                    constant$tpeCost |> 
                      dplyr$filter(value == curValue) |> 
                      dplyr$select(cumCost) |> unlist() |> 
                      replace_na(0)
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
      })
      
      
      bankedTPE <- shiny$reactive({
        appliedTPE <- 
          map(
            .x = constant$attributes$attribute |> str_remove_all(" "),
            .f = function(currentAtt){
              curValue <- input[[currentAtt]]
              
              constant$tpeCost |> 
                dplyr$filter(value == curValue) |> 
                dplyr$select(cumCost) |> 
                unlist()
            }
          ) |> 
          unlist() |> 
          sum()
        
        350 - appliedTPE + 2*(constant$tpeCost$cumCost |> max())
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
    }
  })
}
