box::use(
  bslib,
  dplyr[filter, mutate, select, case_when, rename_with, intersect],
  glue[glue],
  lubridate[today],
  purrr[imap],
  reactable[renderReactable, reactableOutput, reactable],
  shiny,
  shinyjs[enable],
  zip[zip],
)

box::use(
  app/logic/constant,
  app/logic/db/login[isBoD, isFileworker],
  app/logic/db/get[getNextGameID, getLeagueIndex, getSeasonalTotal],
  app/logic/import[parseFMdata],
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
    
    #### OUTPUT UI ####
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())){
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isBoD(auth$usergroup) | isFileworker(auth$usergroup)){
      output$ui <- shiny$renderUI({
        shiny$tagList(
          shiny$h4("Upload and Update Game Statistics"),
          shiny$p(
            paste(
              "Export the Player Statistics for Export view from the League Observer Manager shortlist after the matchday you want to upload.",
              "In case you do not have the view for the League Observer Manager, download and import the view via", 
              shiny$tags$a("this link.", href = "https://drive.google.com/open?id=1b4yz5gkXN6BFSvDBigL3Tp2pUBbu-PJ3&usp=drive_fs", target = "_blank"),
              "The view should be exported as an .HTML file and make sure to scroll through every player in the shortlist as FM only exports players it has 'seen'."
            ) |> 
              shiny$HTML()
          ),
          bslib$layout_column_wrap(
            width = NULL,
            style = bslib$css(grid_template_columns = "1fr 2fr"),
            shiny$div(
              shiny$fileInput(
                inputId = ns("fm"),
                label = "Upload the exported view ",
                accept = ".html"
              ),
              shiny$selectInput(
                inputId = ns("season"),
                label = "Which season is the view from?",
                choices = 1:constant$currentSeason$season,
                selected = constant$currentSeason$season
              )
            ),
            ""
          ),
          bslib$layout_column_wrap(
            width = NULL,
            style = bslib$css(grid_template_columns = "2fr 1fr"),
            shiny$div(
              shiny$p("If the data is correctly imported, the table should show 
                    90 minutes played on average for outfield and goalkeepers."),
              shiny$h5("Checking data"),
              reactableOutput(ns("outputMinutes")),
              shiny$h5("Players per team"),
              reactableOutput(ns("outputPlayers")),
              shiny$h5("Keeper Data"),
              reactableOutput(ns("keeperCheck")),
              shiny$h5("Outfield Data"),
              reactableOutput(ns("outfieldCheck"))
            ),
            ""
          ),
          shiny$div(
            class = "frozen-bottom",
            if(filePath() |> is.null()){
              NULL
            } else {
              req(input$fm)
              shiny$actionButton(
                ns("uploadData"),
                label = "Upload game data"
              )
            }
          )
        )
      })
    } else {
      # TODO ADD NOTE THAT YOU HAVE NO ACCESS
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    }
    
    #### OUTPUT SERVER ####
    
    ## REACTIVES
    filePath <- shiny$reactive({
      file <- input$fm
      
      enable("uploadData")
      
      file$datapath
    })
    
    nextGames <- shiny$reactive({
      req(input$season)
      
      getNextGameID(input$season)
    }) |> 
      shiny$bindEvent(input$fm)
    
    processedGame <- shiny$reactive({
      req(input$fm)
      
      currentSave <- parseFMdata(filePath())
      
      keeperTotals <- getSeasonalTotal(season = input$season, outfield = FALSE)
      outfieldTotals <- getSeasonalTotal(season = input$season)
      
      current <- 
        currentSave |> 
        rename_with(
          .cols = !c(Acc:`Right Foot`, Position, Won, Lost, Drawn),
          .fn = function(x){
            intersect(x |> str_to_lower(),
                      c(colnames(outfieldTotals), colnames(keeperTotals)) |>  
                        unique())
          } 
        ) |> 
        relocate(
          c(colnames(outfieldTotals), colnames(keeperTotals)) |> 
            unique()
        ) |> 
        rename_with(str_to_lower)
      
      print(current)
    })
    
    ## OUTPUT
    
  })
}
