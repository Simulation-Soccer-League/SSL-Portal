box::use(
  bslib,
  dplyr,
  purrr[map],
  shiny,
  shiny.router[change_page],
  stringr[str_remove_all, str_to_title],
  tidyr[pivot_longer, replace_na],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[eligibleRedist, eligibleReroll],
  app/view/tracker/player,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  shiny$tagList(
    shiny$textOutput(ns("text")),
    shiny$uiOutput(ns("attributes"))  
  )
  
}

#' @export
server <- function(id, auth, updated, type) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())){
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)){
      # TODO ADD NOTE THAT YOU HAVE NO ACTIVE PLAYER
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else {
      #### OUTPUT UI ####
      output$attributes <- shiny$renderUI({
        map(
          .x = processedData()$group |> unique() |> sort(),
          .f = function(chosengroup){
            output[[chosengroup]] <- shiny$renderUI({
              temp <- 
                processedData() |> 
                dplyr$filter(
                  group == chosengroup
                ) |> 
                dplyr$select(Attribute, Value)
              
              lapply(
                temp$Attribute,
                FUN = function(currentAtt){
                  value <- 
                    temp |> 
                    dplyr$filter(Attribute == currentAtt) |> 
                    dplyr$select(Value) |> 
                    unlist()
                  
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
                      value = value,
                      min = value,
                      max = 20
                    ),
                    shiny$uiOutput(ns(paste0("cost", currentAtt |> str_remove_all(" "))))
                  )
                }
              )
            })
          }
        )
        
        map(
          .x = processedData()$Attribute,
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
        
        map(
          .x = processedData()$group |> unique() |> sort(),
          .f = function(chosengroup){
            shiny$tagList(
              shiny$div(
                style = "width: 80%",
                shiny$uiOutput(ns(chosengroup))   
              )
            )
          }
        ) |> 
          shiny$div(class = "attributeUpdate")
      })
      
      
      output$text <- shiny$renderText({
        bankedTPE()
      })
      #### OUTPUT SERVER ####
      
      #### REACTIVES ####
      playerData <- shiny$reactive({
        getActivePlayer(auth$uid) |> 
          getPlayer() 
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
            .x = processedData()$Attribute |> str_remove_all(" "),
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
        
        playerData()$tpe - appliedTPE + 2*(constant$tpeCost$cumCost |> max())
      }) |> 
        shiny$bindEvent(
          lapply(
            X = processedData()$Attribute |> str_remove_all(" "),
            FUN = function(x){
              input[[x]]
            }
          ),
          ignoreInit = TRUE
        )
      
      #### OBSERVERS ####
    }
  })
}
