box::use(
  dplyr,
  bslib,
  reactable[reactable, reactableOutput, renderReactable],
  shiny,
)

box::use(
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/constant,
  app/logic/db/get[getAcademyIndex],
  app/logic/ui/tags[flexRow, flexCol],
  app/logic/ui/reactableHelper[recordReactable, indexReactable],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 5fr"),
          shiny$selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              c(
                13:constant$currentSeason$season |> 
                  sort(decreasing = TRUE)
              )
          ),
          ""
        )
      ),
      bslib$card_body(
        shiny$tabsetPanel(
          header = shiny$h1("Outfield"),
          shiny$tabPanel("Statistics",
                         reactableOutput(ns("outfieldBasic")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Adv. Statistics",
                         reactableOutput(ns("outfieldAdvanced")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Leaders",
                         shiny$uiOutput(ns("outfieldLeaders")) |> 
                           withSpinnerCustom(height = 80))
        ),
        shiny$tabsetPanel(
          header = shiny$h1("Keeper"),
          shiny$tabPanel("Statistics",
                         reactableOutput(ns("keeperBasic")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Adv. Statistics",
                         reactableOutput(ns("keeperAdvanced")) |> 
                           withSpinnerCustom(height = 80)),
          shiny$tabPanel("Leaders",
                         shiny$uiOutput(ns("keeperLeaders")) |> 
                           withSpinnerCustom(height = 80))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      outfieldData <- shiny$reactive({
        season <- input$selectedSeason
        
        getAcademyIndex(season = season)
      }) |> 
        shiny$bindEvent(input$selectedSeason)
      
      keeperData <- shiny$reactive({
        season <- input$selectedSeason
        
        getAcademyIndex(season = season, outfield = FALSE)
      }) |> 
        shiny$bindEvent(input$selectedSeason)
      
      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        currentData <- 
          outfieldData() |> 
          dplyr$select(
            name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`
          ) 
        
        currentData |> 
          indexReactable()
      }) |> 
        shiny$bindCache(input$selectedSeason)  
      
      output$outfieldAdvanced <- renderReactable({
        currentData <- 
          outfieldData() |> 
          dplyr$select(
            name:club, 
            xg,
            xa:`fk shots`,
            `open play key passes`:`goals outside box`,
            `press%`:`pen adj xG`
          ) 
        
        currentData |> 
          indexReactable()
      })  |> 
        shiny$bindCache(input$selectedSeason)
      
      output$keeperBasic <- renderReactable({
        currentData <- 
          keeperData() |> 
          dplyr$select(
            name:`save%`
          ) 
        
        currentData |> 
          indexReactable()
      }) |> 
        shiny$bindCache(input$selectedSeason)
      
      output$keeperAdvanced <- renderReactable({
        currentData <- 
          keeperData() |> 
          dplyr$select(
            name:club, 
            `penalties faced`:`xg prevented`
          ) 
        
        currentData |> 
          indexReactable()
      }) |> 
        shiny$bindCache(input$selectedSeason) 
      
    }
  )
}
