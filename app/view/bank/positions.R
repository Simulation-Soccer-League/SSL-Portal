box::use(
  bslib,
  dplyr,
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  sortable[add_rank_list, bucket_list],
  stringr[str_detect, str_remove, str_to_upper],
  tidyr[pivot_longer],
  utils[combn],
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
server <- function(id, cost, playerData, parentSession) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    findSmallestZeroSubset <- function(summary) {
      # separate negatives and non-negatives
      negIdx <- which(summary$difference < 0)
      posIdx <- which(summary$difference >= 0)
      
      negSum <- sum(summary$difference[negIdx])
      
      # if there are no negatives, nothing to do
      if (length(negIdx) == 0) {
        return(list(
          subset    = summary |> dplyr$slice(0),
          remainder = summary
        ))
      }
      
      # otherwise, search for a subset of positives that balances the negatives
      needed <- -negSum
      nPos <- length(posIdx)
      
      for (k in 1:nPos) {
        idxs <- combn(posIdx, k, simplify = FALSE)
        zeroSets <- Filter(function(idx) sum(summary$difference[idx]) == needed, idxs)
        
        if (length(zeroSets) > 0) {
          bestIdx <- zeroSets[[1]]  # first match; could add tie-breakers
          subset <- summary |> dplyr$slice(c(negIdx, bestIdx))
          remainder <- summary |> dplyr$slice(-c(negIdx, bestIdx))
          
          return(list(
            subset    = subset,
            remainder = remainder
          ))
        }
      }
      
      # if no balancing subset found, return just the negatives
      subset <- summary |> dplyr$slice(0)
      remainder <- summary
      
      return(list(subset = subset, 
                  remainder = remainder))
    }
    
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
        ) 
      
      xpOld <- sum(summary$value.old)
      xpNew <- sum(summary$value.new)
      
      if (xpOld > xpNew) {
        showToast(
          .options = constant$myToastOptions,
          "error",
          "You cannot only remove positions from your build, please adjust your 
          positional purchase."
        )
        
        shiny$updateActionButton(
          session = parentSession,
          inputId = "verifyPurchase", 
          label = "You cannot only remove positions!"
        )
      } else {
        summary <- 
          summary |> 
          dplyr$filter(value.old != value.new)
        
        ## If you've only swapped positions
        if (xpOld == xpNew) {
          (nrow(summary) / 2) |> 
            ceiling() |> 
            prod(5E6) |> 
            cost()
        } else {
          difference <- xpNew - xpOld
          
          summary <- 
            summary |> 
            dplyr$mutate(
              difference = value.new - value.old
            )
          
          result <- findSmallestZeroSubset(summary)
          
          swapCost <- 
            (nrow(result$subset) / 2) |> 
            ceiling() |> 
            prod(5E6)
          
          standardCost <- 
            result$remainder |> 
            dplyr$mutate(
              cost = 
                dplyr$case_when(
                  difference == 20 ~ 2E7,
                  difference == 15 ~ 1.25E7,
                  difference == -5 ~ -2.5E6,
                  TRUE ~ 7.5E6 
                )
            )
          
          cost(swapCost + sum(standardCost$cost))
        }
        
        shiny$updateActionButton(
          session = parentSession,
          inputId = "verifyPurchase", 
          label = "Verify purchase"
        )
      }
      
    }) |> 
      shiny$bindEvent(input$pos, ignoreInit = TRUE)
    
    return(input)
  })
}
