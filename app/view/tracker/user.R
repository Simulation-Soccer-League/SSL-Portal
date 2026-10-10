box::use(
  bslib,
  dplyr,
  lubridate[as_date, as_datetime, floor_date, today],
  plotly,
  purrr,
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  scales[comma],
  shiny,
  shiny.router[get_query_param, route_link],
  stringr[
    str_remove, 
    str_split, 
    str_to_lower,
    str_to_upper
  ],
  tidyr[complete, pivot_longer],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/get/getIndex[
    getLatestGames,
    getLeagueIndex,
  ],
  app/logic/get/getPlayer[
    getActivePid,
    getBankHistory,
    getPlayer,
    getPlayerAttributes,
    getTpeHistory, 
    getUpdateHistory
  ],
  app/logic/get/getUser[
    getUserAwards,
    getUserInformation,
    getUserPlayers,
  ],
  app/logic/ui/reactableHelper[
    indexReactable,
    playerCareerReactable,
    linkOrganization,
    recordReactable,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/ui/uiObjects[infoBox],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$layout_column_wrap(
      style = bslib$css(grid_template_columns = "1fr 2fr"),
      heights_equal = "row",
      bslib$card(
        bslib$card_header(
          shiny$uiOutput(ns("userHeader")) |> 
            withSpinnerCustom(height = 100)
        )
      ),
      bslib$card(
        bslib$card_header(
          "User information"
        ),
        bslib$card_body(
          shiny$uiOutput(ns("userInfo")) |>
            withSpinnerCustom(height = 400)
        )
      )
    ),
    bslib$accordion(
      id = ns("accordion"),
      bslib$accordion_panel(
        title = "User Awards",
        shiny$uiOutput(ns("awards"))
      ),
      bslib$accordion_panel(
        title = "Players",
        shiny$uiOutput(ns("players"))
      )
    )
  )
}

#' @export
server <- function(id, uid = NULL, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    query <- shiny$reactive({
      if (uid |> is.null()) {
        uid <- get_query_param("uid")
        
        if (is.null(uid)) {
          NULL
        } else {
          uid |>
            as.numeric()
        }
      } else {
        uid
      }
    })
    
    userInformation <- shiny$reactive({
      shiny$req(query())
      
      getUserInformation(query())
    })
    
    userAwards <- shiny$reactive({
      shiny$req(query())
      
      getUserAwards(query())
    })
    
    players <- shiny$reactive({
      shiny$req(query())
      
      getUserPlayers(query())
    })
    
    awardCard <- function(award) {
      shiny$div(
        class = "award-card",
        style = "
            display: flex;
            align-items: center;
            gap: 1rem;
            padding: 1rem;
            background: var(--background-color);
            border-radius: 12px;
            border: 1px solid #374151;
          ",
        shiny$img(
          src = gsub("\\{homeurl\\}", "https://forum.simulationsoccer.com/", award$awardImage),
          style = "
              width: 40px;
              height: 40px;
              object-fit: contain;
              flex-shrink: 0;
            "
        ),
        
        shiny$div(
          style = "
              display: flex;
              flex-direction: column;
              gap: 0.25rem;
            ",
          shiny$strong(award$awardName),
          shiny$span(
            style = "font-size: 1rem;",
            award$awardDescription
          ),
          if (nzchar(award$awardReason)) {
            shiny$span(
              style = "
                font-size: 0.9rem;
                color: var(--ssl-gold);
                font-style: italic;
              ",
              award$awardReason
            )
          }
        )
      )
    }
    
    playerCard <- function(player) {
      # Player career statistics
      if (player$pos_gk == 20) {
        matches <- getLeagueIndex(
          outfield = FALSE,
          season = "ALL",
          league = "ALL",
          name = player$name,
          career = TRUE
        )
      } else {
        matches <- getLeagueIndex(
          outfield = TRUE,
          season = "ALL",
          league = "ALL",
          name = player$name,
          career = TRUE
        )
      }
      
      # Prepare the career statistics
      if (!is_empty(matches)) {
        career_stats <- matches |>
          dplyr$relocate(season = max_season) |>
          dplyr$arrange(dplyr$desc(season)) |>
          dplyr$select(!name) |>
          playerCareerReactable()
      } else {
        career_stats <- shiny$tags$p(
          "No career statistics available."
        )
      }
      
      # Create one accordion panel per player
      bslib$accordion_panel(
        title = player$name,
        
        shiny$actionButton(
          inputId = session$ns(player$name),
          label = shiny$tags$p(
            shiny$tags$a(
              href = route_link(paste0("tracker/player?pid=", player$pid)),
              "Link to player page"
            )
          ),
          style = paste0("background: ", constant$blue)
        ),
        shiny$h4("Career Statistics"),
        career_stats
      )
    }

    #### Output ####
    output$userHeader <- shiny$renderUI({
      data <- userInformation()
      
      shiny$div(
        class = "flex-row flex-center",
        style = "text-align: left;",
        shiny$div(
          shiny$p(
            style = "
              margin-bottom: 10px; 
              font-weight: 800; 
              font-size: 3.5rem;
            ",
            data$username
          ),
          shiny$h5(
            sprintf("Joined: %s", data$joined |> as_date())
          )
        ),
        shiny$div(
          if (nzchar(data$avatar)){
            shiny$img(
              src = gsub("\\./", "https://forum.simulationsoccer.com/", data$avatar),
              style = "
              width: 64px;
              height: 64px;
              object-fit: contain;
              flex-shrink: 0;
            "
            )
          }
        )
      )
    })
    
    output$userInfo <- shiny$renderUI({
      data <- userInformation()
      
      shiny$div(
        class = "flex-row",
        style = "gap: 1rem;",
        shiny$div(
          style = "
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(min(100px, 100%), 1fr));
              gap: 1.6rem;
              margin: 0;
              padding: 0;
              width: 100%;
            ",
          infoBox("#Posts", data$postnum),
          infoBox("#Threads", data$threadnum),
          infoBox("Reputation", data$reputation),
          infoBox("User Status", shiny$span(class = data$statusDescription |> tolower(), data$statusDescription)),
          infoBox("Discord", data$discord)
        )
      )
      #       shiny$h5("Bank balance:", paste0("$", comma(data$bankBalance)))
      #     )
      #   )
      # )
    }) 
    
    output$players <- shiny$renderUI({
      data <- players()
      
      bslib$accordion(
        lapply(
          seq_len(nrow(data)),
          \(i) playerCard(data[i,])
        )
      )
    })
    
    output$awards <- shiny$renderUI({
      data <- userAwards()
      
      shiny$div(
        class = "flex-row",
        style = "
          gap: 1rem;
          padding-top: 10px;
          max-height: 400px;
          overflow-y: auto;
          justify-content: center;
        ",
        shiny$div(
          style = "
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(min(300px, 100%), 1fr));
              gap: 1rem;
              width: 95%;
            ",
          
          lapply(
            seq_len(nrow(data)),
            \(i) awardCard(data[i, ])
          )
        )
      )
    })
    
    ## Adds code to keep the players running even while it is not shown in ui
    shiny$outputOptions(
      output,
      "players",
      suspendWhenHidden = FALSE
    )
    
    shiny$outputOptions(
      output,
      "awards",
      suspendWhenHidden = FALSE
    )
    
  })
}
