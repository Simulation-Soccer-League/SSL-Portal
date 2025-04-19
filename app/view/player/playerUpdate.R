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
    shiny$uiOutput(ns("ui"))  
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
      output$ui <- shiny$renderUI({
        data <- playerData()
        
        processedData <- 
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
        
        map(
          .x = processedData$group |> unique() |> sort(),
          .f = function(chosengroup){
            output[[chosengroup]] <- shiny$renderUI({
              temp <- 
                processedData |> 
                dplyr$filter(
                  group == chosengroup
                ) |> 
                dplyr$select(Attribute, Value)
              
              lapply(
                temp$Attribute,
                FUN = function(attribute){
                  shiny$tagList(
                    shiny$numericInput(
                      inputId = ns(attribute |> str_remove_all(" ")),
                      label = attribute,
                      value = temp |> 
                        dplyr$filter(Attribute == attribute) |> 
                        dplyr$select(Value) |> 
                        unlist(),
                      min = 5,
                      max = 20
                    ),
                    shiny$uiOutput(ns(paste0("cost", attribute |> str_remove_all(" "))))
                  )
                }
              )
            })
          }
        )
        
        map(
          .x = processedData$Attribute,
          .f = function(attribute){
            output[[paste0("cost", attribute |> str_remove_all(" "))]] <- shiny$renderUI({
              curValue <- input[[attribute |> str_remove_all(" ")]]
              
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
            })
              
          }
        )
        
        map(
          .x = processedData$group |> unique() |> sort(),
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
      
      #### OUTPUT SERVER ####
      
      #### REACTIVES ####
      playerData <- shiny$reactive({
        getActivePlayer(auth$uid) |> 
          getPlayer()
      })
      
      #### OBSERVERS ####
    }
  })
}
