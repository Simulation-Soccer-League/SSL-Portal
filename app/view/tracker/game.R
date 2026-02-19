box::use(
  bslib,
  dplyr,
  lubridate[as_date, as_datetime, floor_date, today],
  plotly,
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  scales[comma],
  shiny,
  shiny.router[get_query_param],
  stringr[
    str_detect, 
    str_remove, 
    str_replace_all,
    str_split, 
    str_to_lower,
    str_to_title,
    str_to_upper
  ],
  tidyr[complete, pivot_longer, pivot_wider],
)

box::use(
  app/logic/constant,
  app/logic/ui/cards[bslibCardContainer],
  app/logic/db/get[
    getGamePlayer,
    getGameSchedule,
    getGameTeam,
    getOrganizations,
    getPreviousGames
  ],
  app/logic/ui/reactableHelper[
    attributeReactable, 
    indexReactable,
    linkOrganization,
    reactableBar,
    recordReactable,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    
    bslib$card(
      bslib$card_body(
        bslib$layout_column_wrap(
          style = bslib$css(grid_template_columns = "3fr 1fr 3fr"),
          shiny$div(
            style = 
              "display: flex; 
              align-items: center; 
              justify-content: center;
              margin: auto;",
            shiny$uiOutput(ns("homeLogo"))
          ),
          shiny$div(
            style = "
              display: flex;
              flex-direction: column;
              align-items: center;
              gap: 36px;
              text-align: center;
            ",
            shiny$uiOutput(ns("competitionName")),
            shiny$div(
              style = "
                font-size: 48px;
                font-weight: 700;
                line-height: 1;
              ",
              shiny$uiOutput(ns("score"))
            ),
            shiny$uiOutput(ns("matchDate"))
          ),
          shiny$div(
            style = 
              "display: flex; 
              align-items: center; 
              justify-content: center;
              margin: auto;",
            shiny$uiOutput(ns("awayLogo"))
          )
        )
      )
    ) |> 
      bslibCardContainer(),
    
    # --- TEAM INFO ----------------------------------------------------
    
    bslib$card(
      bslib$card_header(shiny$h3("Team Statistics")),
      bslib$card_body(
        shiny$uiOutput(ns("teamOverview"))
      )
    ) |> 
      bslibCardContainer(),
    
    # --- PLAYER STATS ---------------------------------------------------------
    bslib$card(
      bslib$card_header(shiny$h3("Player Statistics")),
      bslib$card_body(
        shiny$uiOutput(ns("playerData"))
      )
    ) |> 
      bslibCardContainer(),
    # --- Previous meetings
    bslib$card(
      bslib$card_header(shiny$h3("Previous Meetings")),
      bslib$card_body(
        shiny$uiOutput(ns("previous"))
      )
    ) |> 
      bslibCardContainer()
  )
  
}

#' @export
server <- function(id, gid = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    query <- shiny$reactive({
      if(gid |> is.null()){
        gid <- get_query_param("gid")
        
        if (is.null(gid)) {
          NULL
        } else {
          gid <- 
            gid |>
            as.numeric()
        }
      } else {
        gid <- gid
      }
      
      gid
    })
    
    organizations <- getOrganizations()

    players <- shiny$reactive({
      shiny$req(query())

      getGamePlayer(query())
    }) |> 
      shiny$bindEvent(query())
    
    teams <- shiny$reactive({
      shiny$req(query())

      getGameTeam(query())
    }) |> 
      shiny$bindEvent(query())
    
    boxScore <- shiny$reactive({
      shiny$req(query())

      getGameSchedule(query())
    }) |> 
      shiny$bindEvent(query())
    
    previousMeetings <- shiny$reactive({
      shiny$req(query())
      shiny$req(boxScore())
      
      getPreviousGames(query(), boxScore()$Home, boxScore()$Away)
    })
    
    #### Output ####
    
    shiny$observe({
      bs <- boxScore()
      
      output$competitionName <- shiny$renderUI({
        shiny$img(
          src = sprintf(
            "static/competition/%s.png", 
            bs$Matchtype |> 
              str_replace_all(" ", "_") |> 
              str_to_lower()
          ),
          style = "height: 80px;",
          alt = bs$Matchtype,
          title = bs$Matchtype
        )
      })
      
      output$matchDate <- shiny$renderUI({
        shiny$tagList(
          bs$Matchday,
          shiny$br(),
          bs$IRLDate,
          shiny$br(),
          sprintf("Season %s", bs$Season)
        )
      })
      
      output$homeLogo <- shiny$renderUI({
        linkOrganization(bs$Home, onlyImg = TRUE, height = 150)
      })
      
      output$awayLogo <- shiny$renderUI({
        linkOrganization(bs$Away, onlyImg = TRUE, height = 150)
      })
      
      output$score <- shiny$renderUI({
        if (any(bs$HomeScore, bs$AwayScore) |> is.na()) {
          " - "
        } else {
          sprintf("%s - %s", bs$HomeScore, bs$AwayScore)  
        }
      })
      
    })
    
    output$playerData <- shiny$renderUI({
      if (teams() |> nrow() == 0) {
        shiny$div(
          style = " 
            padding: 24px; 
            text-align: center; 
            background: var(--bottom-background);
            border-radius: 8px; 
            font-size: 16px; 
            font-weight: 500; 
          ", 
          "No player statistics are available for this match."
        )        
      } else {
        shiny$tagList(
          shiny$h3(boxScore()$Home),
          shiny$h5("Players"),
          reactableOutput(session$ns("homePlayers")) |> 
            withSpinnerCustom(height = 200),
          shiny$h5("Goalkeeper"),
          reactableOutput(session$ns("homeKeeper")) |> 
            withSpinnerCustom(height = 200),
          shiny$h3(boxScore()$Away),
          shiny$h5("Players"),
          reactableOutput(session$ns("awayPlayers")) |> 
            withSpinnerCustom(height = 200),
          shiny$h5("Goalkeeper"),
          reactableOutput(session$ns("awayKeeper")) |> 
            withSpinnerCustom(height = 200)
        )   
      }
    })
    
    output$teamOverview <- shiny$renderUI({
      if (teams() |> nrow() == 0) {
        shiny$div(
          style = " 
            padding: 24px; 
            text-align: center; 
            background: var(--bottom-background); 
            border-radius: 8px; 
            font-size: 16px; 
            font-weight: 500; 
          ", 
          "No team statistics are available for this match."
        )        
      } else {
        reactableOutput(session$ns("teamStats")) |> 
          withSpinnerCustom(height = 120)   
      }
    })
    
    output$previous <- shiny$renderUI({
      if (previousMeetings() |> nrow() == 0) {
        shiny$div(
          style = " 
            padding: 24px; 
            text-align: center; 
            background: var(--bottom-background); 
            border-radius: 8px; 
            font-size: 16px; 
            font-weight: 500; 
          ", 
          "No previous meetings between the two teams."
        )        
      } else {
        reactableOutput(session$ns("previousTable")) |> 
          withSpinnerCustom(height = 120)   
      }
    })
    
    output$teamStats <- renderReactable({
      if (teams() |> nrow() == 0) {
        NULL
      } else {
        data <- 
          teams() |> 
          dplyr$select(
            club,
            `distance run (km)`,
            `average rating`,
            xg,
            `shots on target`,
            shots,
            `attempted passes`,
            `pass%`,
            `yellow cards`,
            `red cards`,
            fouls,
            offsides
          ) |> 
          pivot_longer(!club) |> 
          pivot_wider(names_from = club) |>
          dplyr$mutate(
            name = name |> str_to_title()
          ) |> 
          dplyr$select(
            home = boxScore()$Home, 
            name, 
            away = boxScore()$Away
          )
        
        
        reactable(
          data,
          columns = list(
            name = colDef(
              name = "",
              align = "center"
            ),
            home = colDef(
              name = boxScore()$Home,
              align = "center",
              cell = function(value, index) {
                total <- sum(data[index, c("home", "away")])
                reactableBar(
                  value, 
                  total, 
                  side = "right",
                  color = organizations |> 
                    dplyr$filter(name == boxScore()$Home) |> 
                    dplyr$pull(primaryColor)
                )
              }
            ),
            away = colDef(
              name = boxScore()$Away,
              align = "center",
              cell = function(value, index) {
                total <- sum(data[index, c("home", "away")])
                reactableBar(
                  value, 
                  total, 
                  side = "left",
                  color = organizations |> 
                    dplyr$filter(name == boxScore()$Away) |> 
                    dplyr$pull(primaryColor)
                )
              }
            )
          ),
          sortable = FALSE,
          pagination = FALSE
        )
      }
    })
    
    output$homePlayers <- renderReactable({
      players() |> 
        dplyr$filter(club == boxScore()$Home) |> 
        dplyr$select(
          pid, name, club, position,
          `minutes played`:`attempted presses`
        ) |> 
        indexReactable(search = FALSE, pagination = FALSE)
    })
    
    output$awayPlayers <- renderReactable({
      players() |> 
        dplyr$filter(club == boxScore()$Away) |> 
        dplyr$select(
          pid, name, club, position,
          `minutes played`:`attempted presses`
        ) |> 
        indexReactable(search = FALSE, pagination = FALSE)
    })
    
    output$homeKeeper <- renderReactable({
      players() |> 
        dplyr$filter(club == boxScore()$Home & !is.na(conceded)) |> 
        dplyr$select(
          pid, name, club, position,
          `minutes played`, `clean sheets`:`xg prevented`
        ) |> 
        indexReactable(search = FALSE, pagination = FALSE)
    })
    
    output$awayKeeper <- renderReactable({
      players() |> 
        dplyr$filter(club == boxScore()$Away & !is.na(conceded)) |> 
        dplyr$select(
          pid, name, club, position,
          `minutes played`, `clean sheets`:`xg prevented`
        ) |> 
        indexReactable(search = FALSE, pagination = FALSE)
    })
    
    output$previousTable <- renderReactable({
      previousMeetings() |> 
        dplyr$select(Date = IRLDate, Season, League = Matchtype, 
                     Matchday, Home, Away, HomeScore, AwayScore,
                     ExtraTime, Penalties) |>
        dplyr$mutate(Result = sprintf("%s - %s", HomeScore, AwayScore),
                     ExtraTime = dplyr$if_else(ExtraTime == 1, "Yes", "No"),
                     Penalties = dplyr$if_else(Penalties == 1, "Yes", "No")) |> 
        dplyr$select(!c(HomeScore, AwayScore)) |> 
        dplyr$relocate(Result, .after = Away) |> 
        reactable(
          sortable = FALSE,
          pagination = TRUE,
          defaultPageSize = 10,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(5, 10, 20),
          columns = list(
            Date = colDef(
              maxWidth = 80
            ),
            Season = colDef(
              maxWidth = 80
            ),
            Matchday = colDef(
              maxWidth = 200
            ),
            League = colDef(
              maxWidth = 100
            ),
            Home = colDef(
              maxWidth = 70,
              cell = function(value) {
                shiny$img(
                  src = sprintf(
                    "static/logo/%s (Custom).png", 
                    value
                  ),
                  style = "height: 30px;",
                  alt = value,
                  title = value
                )
              }
            ),
            Away = colDef(
              maxWidth = 70,
              cell = function(value) {
                shiny$img(
                  src = sprintf(
                    "static/logo/%s (Custom).png", 
                    value
                  ),
                  style = "height: 30px;",
                  alt = value,
                  title = value
                )
              }
            )
          )
        )
        
    })
    
    
    
  })
    
}
