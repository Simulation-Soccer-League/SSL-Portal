box::use(
  bslib,
  bsicons[bs_icon],
  dplyr,
  purrr[map, is_empty],
  reactable[reactable],
  rlang[`!!!`],
  shiny,
  shinyFeedback[feedback, feedbackDanger, feedbackWarning, hideFeedback, showToast],
  shinyjs,
  shiny.router[change_page],
  sortable[add_rank_list, bucket_list],
  stringr[str_remove_all, str_split, str_to_lower, str_to_title, str_to_upper],
  tidyr[pivot_longer, replace_na],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/logFunctions[logRedist, logReroll],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updatePlayerData],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[
    checkDuplicatedNames, 
    eligibleRedist, 
    eligibleReroll, 
    updateSummary
  ],
  app/logic/ui/tags[flexCol, flexRow, numericStepper],
  app/view/tracker/player,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  shiny$tagList(
    shiny$uiOutput(ns("ui"))  
  )
  
}

#' @export
server <- function(id, auth, updated, type, player = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      shiny$tagList(
        if (type %in% c("reroll", "redistribution")) {
          shiny$tagList(
            if (type == "reroll") {
              shiny$p("Some fields are currently disabled. If you want to change 
            your name or render as part of your re-roll, 
            please reach out to a member of the BoD")  
            },
            shiny$uiOutput(ns("playerInfo"))  
          )
        },
        shiny$uiOutput(ns("roleSelector")),
        shiny$uiOutput(ns("attributes")),
        if(type %in% c("reroll", "redistribution")){
          shiny$uiOutput(ns("outfieldExtras"))
        },
        bslib$layout_column_wrap(
          width = 1/3,
          shiny$textOutput(ns("tpeRemaining")),
          shiny$actionButton(
            ns("verifyUpdate"),
            label = 
              dplyr$if_else(
                type == "update", "Update",
                dplyr$if_else(
                  type == "regression", "Regress",
                  dplyr$if_else(
                    type == "reroll", "Reroll", "Redistribute"
                  )
                )),
            class = "primary-button"
          ),
          shiny$actionButton(
            ns("back"),
            label = "Back",
            class = "primary-button"
          )
        ) |> 
          shiny$div(class = "frozen-bottom"),
        shiny$div(style = "min-height:100px;")
      )
    })
    
    output$attributes <- shiny$renderUI({
      if(type %in% c("reroll", "redistribution")){
        groups <- attributeGroups()
        attributes <- constant$attributes$attribute
      } else {
        groups <- processedData()$group |> unique() |> sort()
        attributes <- processedData()$Attribute
      }
      
      map(
        .x = groups,
        .f = function(chosengroup){
          output[[chosengroup]] <- shiny$renderUI({
            if (type %in% c("update", "regression")) {
              temp <- 
                processedData() |> 
                dplyr$filter(
                  group == chosengroup
                ) |> 
                dplyr$select(Attribute, Value)
            } else if (type == "redistribution" & !gkReroll()) {
              temp <- 
                constant$attributes |> 
                dplyr$filter(
                  if (input$playerType == "Goalkeeper"){
                    group == chosengroup & keeper == TRUE
                  } else {
                    group == chosengroup
                  }
                ) |> 
                dplyr$select(Attribute = attribute) |> 
                dplyr$left_join(
                  processedData(),
                  by = "Attribute"
                ) |> 
                dplyr$mutate(
                  Value = dplyr$if_else(Value |> is.na(), 5, Value)
                )
              
            } else {
              temp <- 
                constant$attributes |> 
                dplyr$filter(
                  if (input$playerType == "Goalkeeper"){
                    group == chosengroup & keeper == TRUE
                  } else {
                    group == chosengroup
                  }
                ) |> 
                dplyr$select(Attribute = attribute) |> 
                dplyr$mutate(Value = 5)
            }
            
            flexCol(
              style = "gap: 12px;",
              lapply(
                temp$Attribute,
                FUN = function(currentAtt){
                  value <- 
                    temp |> 
                    dplyr$filter(Attribute == currentAtt) |> 
                    dplyr$select(Value) |> 
                    unlist()
                  
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
                            value = value,
                            min = dplyr$if_else(type == "update", value, 5),
                            max = 
                              dplyr$if_else(
                                currentAtt %in% c("Acceleration", "Agility", "Balance", "Jumping Reach", "Pace", "Strength"),
                                dplyr$if_else(
                                  type == "regression", 
                                  min(value, 20 - max(playerData()$timesregressed - 2, 0)), 
                                  20 - max(playerData()$timesregressed - 2, 0)
                                ),
                                dplyr$if_else(
                                  type == "regression", 
                                  value, 
                                  20
                                )
                              )
                          )
                        )
                      ),
                      shiny$uiOutput(ns(paste0("cost", attID))),
                    )
                  }
                }
              )
            )
          })
        }
      )
      
      map(
        .x = attributes,
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
          .x = groups,
          .f = function(chosengroup){
            flexCol(
              style = "height: fit-content; border: 1px solid white; border-radius: 4px; padding: 12px;",
              shiny$tagList(
                shiny$span(paste0(chosengroup, " Attributes")),
                shiny$uiOutput(ns(chosengroup))
              )
            )
          }
        )
      
      flexRow(
        style = "gap: 24px; margin-bottom: 100px;",
        uiList
      )
    })
    
    output$playerInfo <- shiny$renderUI({
      bslib$layout_column_wrap(
        width = 1/3,
        shiny$textInput(
          ns("firstName"), 
          "Enter a first name:", 
          value = playerData()$first,
          placeholder = "First Name"
          # ) |> 
          #   (\(x) if (type == "redistribution") shinyjs$disabled(x) else x)(),
        ) |> 
          shinyjs$disabled(),
        shiny$textInput(
          ns("lastName"), 
          "*Enter a last name:", 
          value = playerData()$last,
          placeholder = "Last Name"
        # ) |> 
        #   (\(x) if (type == "redistribution") shinyjs$disabled(x) else x)(),
        ) |> 
          shinyjs$disabled(),
        shiny$selectInput(
          ns("pronouns"), 
          tippy(
            "*Select your player's pronouns",
            "This is used by commentators to correctly gender your player. Multiple selections are allowed",
            theme = "ssl"
          ),
          choices = c("He/Him", "She/Her", "They/Them"),
          selected = playerData()$pronouns |> str_split(constant$traitSep) |> unlist(),
          multiple = TRUE
        ),
        shiny$textInput(
          ns("birthplace"), 
          tippy("Enter a place of birth:", "Only thematic", theme = "ssl"), 
          value = playerData()$birthplace,
          placeholder = "City"
        ) |> 
          (\(x) if (type == "redistribution") shinyjs$disabled(x) else x)(),
        shiny$selectInput(
          ns("nationality"), 
          tippy("Select a nationality:", "Cannot be changed with a reroll or redistribution.", theme = "ssl"),
          choices = constant$sslNations,
          ## Cannot change nationality with a redistribution or reroll
          selected = playerData()$nationality
        ) |> 
          shinyjs$disabled(),
        shiny$numericInput(
          ns("height"), 
          tippy("Enter height (inches):", "Only cosmetic", theme = "ssl"), 
          value = playerData()$height, 
          min = 55, 
          max = 90
        ),
        shiny$numericInput(
          ns("weight"), 
          tippy("Enter weight (pounds):", "Only cosmetic", theme = "ssl"), 
          value = playerData()$weight, 
          min = 100, 
          max = 350, 
          step = 5
        ),
        shiny$selectInput(
          ns("footedness"), 
          "*Select preferred foot:", 
          choices = c("", "Left", "Right"),
          selected = dplyr$if_else(playerData()$`left foot` == 20, "Left", "Right")
          ),
        shiny$textInput(
          ns("render"), 
          tippy("Select a player likeness:", 
                "A player render is a person or thing you want your player to look like for graphics used within the league.",
                theme = "ssl"), 
          value = playerData()$render,
          placeholder = "ex. Lionel Messi"
        ) |> 
          shinyjs$disabled(),
        "",
        if(type == "reroll"){
          shiny$radioButtons(
            ns("playerType"), 
            "Outfield or Goalkeeper", 
            choices = c("Outfield", "Goalkeeper"), 
            selected = dplyr$if_else(playerData()$pos_gk == 20,
                                     "Goalkeeper",
                                     "Outfield"), 
            inline = TRUE
          )
        } else {
          shiny$radioButtons(
            ns("playerType"), 
            "Outfield or Goalkeeper", 
            choices = c("Outfield", "Goalkeeper"), 
            selected = dplyr$if_else(playerData()$pos_gk == 20,
                                     "Goalkeeper",
                                     "Outfield"), 
            inline = TRUE
          ) |> 
            shinyjs$disabled()
        },
        if (type == "redistribution" & 
            playerData()$pos_gk == 20) {
          shiny$radioButtons(
            ns("gkReroll"),
            "Do you want to reroll to an outfielder as part of your
            redistribution?",
            choices = c("Yes" = TRUE, "No" = FALSE),
            selected = FALSE,
            inline = TRUE
          )
        } else {
          ""  
        }
      )
    })
    
    output$roleSelector <- shiny$renderUI({
      shiny$tagList(
        paste("Football Manager uses <i>roles</i> and <i>duties</i> to control what your player will do within a
      set tactic. There exists many different roles with different importances given to specific
      attributes. Selecting a role and duty you want to build towards in the list below will highlight the <span
      class='keyAttribute'>very important</span> and
      <span class='importantAttribute'>important</span>          attributes.") |> shiny$HTML(),
        shiny$br(),
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 2fr 2fr"),
          shiny$selectInput(
            ns("selectedRole"), 
            label = tippy("Select a player role", 
                          tooltip = "Please note, the role you play will be determined by your Manager. If you want to play a specific role, make sure to speak with your Manager.",
                          theme = "ssl"), 
            choices = names(constant$roleAttributes)),
          if (playerData()$timesregressed > 0) {
            bslib$value_box(
              style = "width: 65%;",
              title = "Maximum Physical Attribute",
              value = min(20 - (playerData()$timesregressed - 2)),
              fill = TRUE,
              theme = 
                bslib$value_box_theme(
                  name = "danger",
                  bg = constant$red,
                  fg = "#000000"
                ),
              showcase = bs_icon("exclamation-triangle-fill")
            )
          } else {
            ""
          },
          ""
        )
      )
    })
    
    output$outfieldExtras <- shiny$renderUI({
      shiny$req(input$playerType)
      
      if(input$playerType == "Outfield") {
        positions <- constant$positions
        
        if(type == "redistribution"){
          currentPositions <- playerData() |> 
            dplyr$select(
              dplyr$contains("pos_")
            ) |> 
            pivot_longer(
              everything()
            ) |> 
            dplyr$mutate(
              name = str_remove_all(name, "pos_") |> 
                str_to_upper()
            )
          
          posPrim <- positions[names(positions) %in% (currentPositions |> 
                                                        dplyr$filter(value == 20) |> 
                                                        dplyr$select(name) |> unlist())
          ]
          
          posSec <- positions[names(positions) %in% (currentPositions |> 
                                                       dplyr$filter(value < 20 & value > 5) |> 
                                                       dplyr$select(name) |> unlist())
          ]
          
          posRem <- positions[!(positions %in% c(posPrim, posSec))]
        } else {
          posPrim <- posSec <- NULL
          posRem <- constant$positions
        }
        
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
              labels = posPrim,
              input_id = ns("primary")
            ),
            add_rank_list(
              text = "Select TWO secondary positions:",
              labels = posSec,
              input_id = ns("secondary")
            ),
            add_rank_list(
              text = "Drag from here",
              labels = posRem,
              input_id = ns("unusedPositions")
            )
          ),
          shiny$checkboxGroupInput(
            ns("traits"), 
            "Select two (2) traits:", 
            choices = constant$traits |> 
              unlist(use.names = FALSE),
            selected = 
              dplyr$if_else(
                type == "redistribution", 
                playerData()$traits |> 
                  str_split(constant$traitSep),
                list("")
              ) |> 
              unlist()
          ) |> 
            shiny$div(class = "multicol")
        )
      }
    })
    
    output$tpeRemaining <- shiny$renderText({
      paste("TPE Remaining: ", bankedTPE())
    })
    #### OUTPUT SERVER ####
    
    #### REACTIVES ####
    playerData <- shiny$reactive({
      if (is.null(player)) {
        getActivePlayer(auth$uid) |> 
          getPlayer()  |> 
          dplyr$mutate(
            nationality = constant$sslNations[nationality] 
          )  
      } else {
        player
      }
    })
    
    gkReroll <- shiny$reactive({
      if (input$gkReroll |> is.null()) {
        FALSE
      } else {
        input$gkReroll == "TRUE"
      }
    })
    
    # Based attributes on the input$playerType
    attributeGroups <- shiny$reactive({
      shiny$req(input$playerType)
      if(type %in% c("reroll", "redistribution")){
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
      }
    })
    
    processedData <- shiny$reactive({
      data <- playerData()
      
      data |> 
        dplyr$select(acceleration:throwing) |> 
        dplyr$select(
          dplyr$where(~ !is.na(.x))
        ) |> 
        pivot_longer(
          cols = dplyr$everything(),
          values_to = "Value",
          names_to = "Attribute"
        ) |> 
        dplyr$mutate(
          Attribute = str_to_title(Attribute)
        ) |> 
        dplyr$left_join(
          constant$attributes,
          by = c("Attribute" = "attribute")
        ) |> 
        dplyr$mutate(
          Attribute = factor(Attribute, levels = sort(Attribute |> unique(), decreasing = TRUE)),
          group = factor(group, levels = c("Physical", "Mental", "Technical", "Goalkeeper"))
        ) |> 
        dplyr$filter(
          if (data$pos_gk == 20){
            (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE") | (group %in% c("Physical", "Mental"))
          } else {
            group %in% c("Physical", "Mental", "Technical")
          }
        ) |>
        dplyr$arrange(group)
    })
    
    bankedTPE <- shiny$reactive({
      appliedTPE <- 
        map(
          .x = constant$attributes$attribute |> str_remove_all(" "),
          .f = function(currentAtt){
            curValue <- input[[currentAtt]]
            
            if(!(curValue |> is_empty())){
              constant$tpeCost |> 
                dplyr$filter(value == curValue) |> 
                dplyr$select(cumCost) |> 
                unlist()
            }
            
          }
        ) |> 
          unlist() |> 
          sum()
      
      playerData()$tpe - appliedTPE + 2*(constant$tpeCost$cumCost |> max())
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
    shiny$observe({
      if (is.null(player)) {
        change_page("myPlayer/")  
      } else {
        showToast(
          .options = constant$sslToastOptions,
          "warning",
          "You can't go back to anywhere."
        )
      }
    }) |> 
      shiny$bindEvent(input$back)
    
    shiny$observe({
      if (gkReroll()) {
        shinyjs$enable(
          id = "playerType"
        )
      } else {
        shinyjs$disable(
          id = "playerType"
        )
      }
    }) |> 
      shiny$bindEvent(input$gkReroll)
    
    ## Verify the update
    shiny$observe({
      currentTraits <- playerData()$traits |> 
        str_split(constant$traitSep) |> 
        unlist()
      
      positions <- constant$positions
      currentPositions <- playerData() |> 
        dplyr$select(dplyr$contains("pos_")) |> 
        pivot_longer(everything()) |> 
        dplyr$mutate(
          name = str_remove_all(name, "pos_") |> 
            str_to_upper()
        )
      
      posPrim <- positions[names(positions) %in% (currentPositions |> 
                                                    dplyr$filter(value == 20) |> 
                                                    dplyr$select(name) |> unlist())
      ]
      
      posSec <- positions[names(positions) %in% (currentPositions |> 
                                                   dplyr$filter(value < 20 & value > 5) |> 
                                                   dplyr$select(name) |> unlist())
      ]
      
      if(type %in% c("redistribution", "reroll")){
        if(bankedTPE() < 0){
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "You have spent too much TPE on your attributes! Reduce some of your attributes and try again."
          )
        } else if((sapply(
          c(input$lastName, input$nationality, input$footedness, 
            input$hairColor, input$hairLength, input$skinColor), 
          FUN = function(x) x == "", simplify = TRUE
        ) |> any()) | 
        is.null(input$pronouns) | 
        (
          input$playerType == "Outfield" & 
          (input$traits |> length() != max(length(currentTraits), 2) | 
           ## This covers both GK (1) or multiple primary positions (OUT)
           input$primary |> length() != max(length(posPrim), 1) | 
           ## The number of secondary positions is either 2 if going from GK to OUT
           ## or the number of secondary positions they currently have
           input$secondary |> length() != 
           dplyr$if_else(playerData()$pos_gk == 20, 2, length(posSec)))
        )){
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "You have missed a required field. Please check the marked fields."
          )
          
          feedbackDanger("lastName", input$lastName == "", "Please enter a last name for your player. If you only want to use one name, please enter it here instead of as a first name.")
          feedbackDanger("nationality", input$nationality == "", "Please enter the nationality of your player.")
          feedbackDanger("footedness", input$footedness == "", "Please enter the footedness of your player.")
          feedbackDanger("hairColor", input$hairColor == "", "Please enter the hair color for your player.")
          feedbackDanger("hairLength", input$hairLength == "", "Please enter the hair length for your player.")
          feedbackDanger("skinColor", input$skinColor == "", "Please enter the skin tone for your player.")
          feedbackDanger("pronouns", is.null(input$pronouns), "Please enter at least one pronoun for your player.")
          
          if(input$traits |> length() != max(length(currentTraits), 2)){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              paste("Please select", max(length(currentTraits), 2), "traits.")
            )
          }
          
          if(input$primary |> length() != max(length(posPrim), 1)){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              paste("You can select", max(length(posPrim), 1), "primary position.")
            )
          }
          
          if(input$secondary |> length() != max(length(posSec), 2)){
            showToast(
              .options = constant$sslToastOptions,
              "error", 
              paste("You can select", max(length(posSec), 2), "secondary position.")
            )
          }
        } else if (checkDuplicatedNames(input$first, input$last)){
          showToast(
            .options = constant$sslToastOptions,
            "error", 
            "Another player in the league's history have used this name. 
          Please change it to something else."
          )
        } else {
          updates <- updateSummary(playerData(), input, type) 
          
          attributes <- updates |> 
            dplyr$filter(
              attribute %in% (constant$attributes$attribute |> str_to_lower())
            ) |> 
            dplyr$mutate(
              dplyr$across(
                c(old, new),
                as.numeric
              )
            ) |> 
            dplyr$left_join(
              constant$tpeCost |> 
                dplyr$select(value, cumCost),
              by = c("old" = "value")
            ) |> 
            dplyr$left_join(
              constant$tpeCost |> 
                dplyr$select(value, cumCost),
              by = c("new" = "value"),
              suffix = c("old", "new")
            ) |> 
            dplyr$mutate(
              diff = cumCostold - cumCostnew
            ) |> 
            dplyr$filter(diff > 0) |> 
            dplyr$summarize(sum = sum(diff))
          
          
          ## Limits the removed tpe changes to 100 if you are only redistributing
          ## Reroll from GK to outfield removes this limit
          if (attributes$sum > 100 & 
             type == "redistribution" & 
             (input$playerType == "Goalkeeper" & gkReroll())) {
            showToast(
              .options = constant$sslToastOptions,
              "error",
              "You have removed more than the allowed 100 TPE in the redistribution."
            )
          } else if (updates |> nrow() == 0) {
            showToast(
              .options = constant$sslToastOptions,
              "warning",
              "You have not changed your build yet, there is nothing to update."
            )
          # } else if() {
            
          } else {
            shiny$showModal(
              shiny$modalDialog(
                title = "Verify your update",
                reactable(updates),
                easyClose = FALSE,
                footer = shiny$tagList(
                  shiny$modalButton("Cancel"),
                  shiny$actionButton(ns("confirmUpdate"), "OK")
                )
              )
            )
          }
        }
      } else {
        if(bankedTPE() < 0){
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "You have spent too much TPE on your attributes! Reduce some of your attributes and try again."
          )
        } else if (type == "regression" & bankedTPE() > 24) {
          
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "You cannot regress more than 24 TPE over your required TPE."
          )
          
        } else {
          updates <- updateSummary(playerData(), input, type)
          
          if(updates |> nrow() == 0){
            showToast(
              .options = constant$sslToastOptions,
              "warning",
              "You have not changed your build yet, there is nothing to update."
            )
          } else {
            shiny$showModal(
              shiny$modalDialog(
                title = "Verify your update",
                reactable(updates),
                easyClose = FALSE,
                footer = shiny$tagList(
                  shiny$modalButton("Cancel"),
                  shiny$actionButton(ns("confirmUpdate"), "OK")
                )
              )
            )
          }
        }
      }
    }) |> 
      shiny$bindEvent(
        input$verifyUpdate,
        ignoreInit = TRUE
      )
    
    shiny$observe({
      shiny$removeModal()
      
      tryCatch({
        updates <- updateSummary(playerData(), input, type)
        
        updatePlayerData(
          uid = auth$uid, 
          pid = playerData()$pid, 
          updates = updates,
          bankedTPE = bankedTPE()
        )
        
        updated(updated() + 1)
        
        if(type == "redistribution"){
          logRedist(pid = playerData()$pid)
        } else if(type == "reroll"){
          logReroll(pid = playerData()$pid)
        }
        
        showToast(
          .options = constant$sslToastOptions,
          "success",
          "You have successfully updated your player!"
        )
        if (is.null(player)) {
          change_page("myPlayer/")  
        }
      }, error = function(e){
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
        input$confirmUpdate,
        ignoreInit = TRUE
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
  })
}
