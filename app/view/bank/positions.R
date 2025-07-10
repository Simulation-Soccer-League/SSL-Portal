box::use(
  bslib,
  dplyr,
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  sortable[add_rank_list, bucket_list],
  stringr[str_detect, str_remove, str_to_upper],
  tidyr[pivot_longer],
)

box::use(
  app/logic/constant,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$h4("Player Traits")
      ),
      bslib$card_body(
        shiny$tagList(
          shiny$p(
            paste(
              "At creation, a player starts with one primary and two secondary positions. 
              With purchases in the Player Store you are able to switch, upgrade or add 
              positions to your player. The items available are:",
              shiny$tags$ul(
                shiny$tags$li("Position Transfer ($5 million): A transfer consists of 
                              swapping two positions between any boxes."),
                shiny$tags$li("Position Upgrade ($7.5 million): An upgrade consists of 
                              moving a position from the secondary to primary box."),
                shiny$tags$li("Position Addition ($12.5 million): An addition consists 
                              of moving a 'new' position to the secondary position box. 
                              If you move a position to the primary box this is an 
                              Addition + Upgrade.")
              )
            ) |> 
              shiny$HTML()
          ),
          shiny$uiOutput(ns("positionBucket")) 
        )
      )
    )
  )
}


#' @export
server <- function(id, cost, playerData, session) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #### Server ####
    output$positionBucket <- shiny$renderUI({
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
    
    #### Reactives ####
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
    
    
    #### Observers ####
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
            value.old == 0  & value.new == 20 ~ "Addition + Upgrade",
            value.old == 0  & value.new == 15 ~ "Addition",
            value.old >= 5  & value.old <  20 & value.new == 20 ~ "Upgrade",
            value.old == 20 & value.new == 15 ~ "Downgrade",
            value.old == 20 & value.new == 0  ~ "Downgrade + Remove",
            value.old >= 5  & value.old <  20 & value.new == 0  ~ "Remove",
            TRUE ~ "No Change"
          )
        ) |> 
        dplyr$group_by(change) |> 
        dplyr$summarize(n = dplyr$n()) |> 
        dplyr$mutate(
          cost = dplyr$case_when(
            change == "Addition + Upgrade" ~ n * 20000000,
            change == "Upgrade" ~ n * 7500000,
            change == "Addition" ~ n * 12500000,
            change == "Downgrade + Remove" ~ -n * 20000000,
            change == "Downgrade" ~ -n * 7500000,
            change == "Remove" ~ -n * 12500000,
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
          swaps = floor((Addition + Remove) / 2) + floor((Upgrade + Downgrade) / 2) + 
            floor((AdditionPlus + RemovePlus) / 2)
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
      if (summary$n |> sum() == 13) {
        cost(sum(summary$cost))
      }
      
      if (summary$cost |> sum() < 0) {
        showToast(
          .options = constant$myToastOptions,
          "error",
          "You cannot only remove positions from your build, please adjust your 
          positional purchase."
        )
        
        shiny$updateActionButton(
          session = session,
          inputId = "verifyPurchase", 
          label = "You cannot only remove positions!"
        )
        
        disable(id = "verifyPurchase")
      } else {
        shiny$updateActionButton(
          session = session,
          inputId = "verifyPurchase", 
          label = "Verify purchase"
        )
        
        enable(id = "verifyPurchase")
      }
    }) |> 
      shiny$bindEvent(input$pos, ignoreInit = TRUE)
  })
}
