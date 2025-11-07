box::use(
  bslib,
  dplyr,
  googlesheets4[gs4_deauth, read_sheet],
  lubridate[as_date],
  reactable[
    colDef, 
    reactable,
    reactableOutput, 
    renderReactable,
  ],
  rlang[is_empty],
  shiny,
  stringr[str_detect, str_split],
  tidyr[pivot_longer],
  tippy[tippy],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getStandings],
  app / logic / ui / selector[leagueSelectInput],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexRow],
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
            choices = constant$currentSeason$season:21
          ),
          ""
        )
      ),
      bslib$card_body(
        shiny$h3("Standings"),
        reactableOutput(ns("standings")) |>
          withSpinnerCustom(height = 80) |> 
          shiny$div(align = "center"),
        shiny$h3("Schedule"),
        reactableOutput(ns("schedule")) |> 
          withSpinnerCustom(height = 80)
      )
    )
  )
}

#' @export
server <- function(id, updated) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      gs4_deauth()
      
      #### DATA GENERATION ####
      schedule <- shiny$reactive({
        
        
        read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
          sheet = paste("Season", input$selectedSeason, "Academy")
        ) |> 
          dplyr$mutate(
            dplyr$across(
              Division:Matchday,
              unlist
            )
          )
      }) |> 
        shiny$bindCache(
          id,
          input$selectedSeason
        ) |> 
        shiny$bindEvent(
          input$selectedSeason
        )

      
      standings <- shiny$reactive({
        sheet <- 
          schedule()
        
        tryCatch({
          sheet |> 
            dplyr$mutate(
              `In-game Date` = `In-game Date` |> as_date() |> format(format = "%m/%d"),
              `IRL Date` = `IRL Date` |> as_date() |> format(format = "%m/%d"),
              HomeScore = 
                str_split(
                  Result, 
                  pattern = "-", 
                  simplify = TRUE
                )[,1], 
              AwayScore = 
                str_split(
                  Result, 
                  pattern = "-", 
                  simplify = TRUE
                )[,2]
            ) |>
            dplyr$mutate(
              HomePoints = 
                dplyr$case_when(
                  str_detect(HomeScore, "e|p") ~ 3,
                  HomeScore |> as.numeric() > AwayScore |> as.numeric() ~ 3,
                  HomeScore |> as.numeric() == AwayScore |> as.numeric() ~ 1,
                  TRUE ~ 0
                ),
              AwayPoints = 
                dplyr$case_when(
                  str_detect(AwayScore, "e|p") ~ 3,
                  HomeScore |> as.numeric() < AwayScore |> as.numeric() ~ 3,
                  HomeScore |> as.numeric() == AwayScore |> as.numeric() ~ 1,
                  TRUE ~ 0
                )
            ) |> 
            dplyr$select(
              -`IRL Date`,
              -`In-game Date`,
              -Result
            ) |> 
            pivot_longer(
              c(HomePoints, AwayPoints),
              names_to = c("set", ".value"),
              names_pattern = "(....)(.*)$"
            ) |> 
            dplyr$mutate(
              Team = 
                dplyr$case_when(
                  set == "Home" ~ Home,
                  TRUE ~ Away
                ),
              GF = 
                dplyr$case_when(
                  set == "Home" ~ HomeScore,
                  TRUE ~ AwayScore
                ),
              GA = 
                dplyr$case_when(
                  set == "Home" ~ AwayScore,
                  TRUE ~ HomeScore
                ),
              Matchday = unlist(Matchday)
            ) |> 
            dplyr$select(
              !dplyr$contains("Home"),
              !dplyr$contains("Away"),
              !set
            ) |> 
            dplyr$filter(
              !is.na(GF) & GF != "",
              !is.na(as.numeric(Matchday) |> suppressWarnings()) 
            ) |> 
            dplyr$group_by(
              Team
            ) |> 
            dplyr$summarize(
              GP = dplyr$n(),
              W = sum(Points == 3),
              D = sum(Points == 1),
              L = sum(Points == 0),
              GF = sum(as.numeric(GF)),
              GA = sum(as.numeric(GA)),
              GD = GF-GA,
              Points = sum(Points)
            ) |> 
            dplyr$arrange(
              dplyr$desc(Points),
              dplyr$desc(GD)
            ) |> 
            dplyr$ungroup() |> 
            dplyr$mutate(
              Pos = seq_len(dplyr$n())
            ) |> 
            dplyr$relocate(
              c(Pos), 
              .before = Team
            ) |> 
            dplyr$rename(
              Pts = Points
            )
        })
        
      })

      #### UI OUTPUT ####
      output$standings <- renderReactable({
        reactable(
          standings(), 
          pagination = FALSE,
          fullWidth = FALSE,
          defaultColDef = colDef(minWidth = 60),
          columns = 
            list(
              Team = colDef(name = "", width = 200, align = "left", cell = function(value){
                image <- shiny$img(
                  src = sprintf("static/logo/%s (Custom).png", value),
                  style = "height: 30px;",
                  alt = value,
                  title = value
                )
                
                list <-
                  shiny$tagList(
                    flexRow(
                      style = "align-items: center; gap: 8px;",
                      shiny$tagList(
                        image,
                        shiny$span(class = "truncated-text", value)
                      )
                    )
                  )
              }),
              GP = colDef(header = tippy("GP", "Games played", placement = "top", theme = "ssl", arrow = TRUE)),
              W = colDef(header = tippy("W", "Wins", placement = "top", theme = "ssl", arrow = TRUE)),
              D = colDef(header = tippy("D", "Draws", placement = "top", theme = "ssl", arrow = TRUE)),
              L = colDef(header = tippy("L", "Losses", placement = "top", theme = "ssl", arrow = TRUE)),
              GF = colDef(header = tippy("GF", "Goals scored", placement = "top", theme = "ssl", arrow = TRUE)),
              GA = colDef(header = tippy("GA", "Goals conceded", placement = "top", theme = "ssl", arrow = TRUE)),
              Pts = colDef(header = tippy("P", "Points", placement = "top", theme = "ssl", arrow = TRUE))
            )
        ) 
      }) 
      
      output$schedule <- renderReactable({
        schedule() |> 
          dplyr$select(Date = `IRL Date`, Matchday, Home, Away, Result) |>
          dplyr$mutate(
            Date = Date |> as_date()
          ) |> 
          reactable(
            pagination = FALSE,
            columns = 
              list(
                Date = colDef(width = 100),
                Matchday = colDef(width = 100)
              )
          )
      })
    }
  )
}
