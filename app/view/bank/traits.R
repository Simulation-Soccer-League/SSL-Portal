box::use(
  bslib,
  shiny,
  stringr[str_split],
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
              "Each new trait costs $3 million and your player can have a maximum of 7 traits, 
              including the 2 you started with. The traits are separated into seven different 
              categories depending on what part of the play they affect. Note that some of the 
              traits will negatively impact your player if their attributes does not match. 
              If you notice a disabled trait in the list, it is not compatible with one or 
              more of your current traits. The full list of incompatible traits can be found 
              in the", 
              shiny$tags$a(
                "Player Handbook.", 
                href = 
                  "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit#", 
                target = "_blank"
              ), "If you want to remove an already selected player trait, you can purchase 
              this item ($0.5 million)."
            ) |> 
              shiny$HTML()
          ),
          shiny$checkboxGroupInput(
            ns("traits"), 
            paste("Change your traits"), 
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
  )
}


#' @export
server <- function(id, cost, playerData) {
  shiny$moduleServer(id, function(input, output, session) {
    
    #### Reactives ####
    currentTraits <- shiny$reactive({
      playerData()$traits |> 
        str_split(constant$traitSep) |> 
        unlist()
    })
    
    nrTraits <- shiny$reactiveVal({
      currentTraits() |> 
        length()
    })
    
    
    #### Observers ####
    ## Marks current traits
    shiny$observe({
      shiny$updateCheckboxGroupInput(
        inputId = "traits",
        selected = currentTraits()
      )
    })
    
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
        disableList <- c(disableList, "Comes Deep To Get Ball", "Stays Back At All Times", 
                         "Hugs Line")
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
      
      # if(length(disableList) == 0){
      #   disableList <- "none"
      # }
      
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
    
    ## Calculates sum of trait purchase
    shiny$observe({
      removed <- sum(!(currentTraits() %in% input$traits))
      added <- sum(!(input$traits %in% currentTraits()))
      
      cost(removed * 500000 + added * 3000000)
    }) |> 
      shiny$bindEvent(
        input$traits,
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      )
    
    return(input)
  })
}
