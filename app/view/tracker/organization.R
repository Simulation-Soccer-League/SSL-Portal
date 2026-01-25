box::use(
  bslib,
  dplyr,
  glue,
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
    str_split, 
    str_to_lower,
    str_to_upper
    ],
  tidyr[complete, pivot_longer],
)

box::use(
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/db/get[
    getBankHistory,
    getLeagueIndex,
    getOrganizationPlayers, 
    getTeamInformation,
  ],
  app/logic/ui/reactableHelper[linkOrganization, orgReactable],
  app/logic/ui/spinner[withSpinnerCustom],
  app / logic / ui / tags[flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$h2("Organization")
      ),
      bslib$card_body(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 3fr"),
          shiny$uiOutput(ns("clubLogo"), height = NULL) |>
            withSpinnerCustom(height = 20),
          shiny$uiOutput(ns("orgInfo")) |>
            withSpinnerCustom(height = 40)
        )
      )
    ),
    bslib$card(
      bslib$card_header(
        shiny$h2("Roster Overview")
      ),
      bslib$card_body(
        shiny$uiOutput(ns("tabs")) |> 
          withSpinnerCustom(height = 50)
      )
    )
    # bslib$layout_column_wrap(
    #   width = 1 / 2,
    #   bslib$card(
    #     bslib$card_header(
    #       shiny$h3("Organization Information")
    #     ),
    #     bslib$card_body(
    #     )
    #   ),
    #   bslib$card(
    #     bslib$card_header(
    #       shiny$h3("Match Statistics")
    #     ),
    #     bslib$card_body(
    #       shiny$tabsetPanel(
    #         shiny$tabPanel(
    #           title = "Last 10 games",
    #           reactableOutput(ns("matchStatistics"))
    #         ),
    #         shiny$tabPanel(
    #           title = "Career Statistics",
    #           reactableOutput(ns("careerStatistics"))
    #         )
    #       )
    #     )
    #   )
    # ),
    # bslib$layout_column_wrap(
    #   width = NULL,
    #   style = bslib$css(grid_template_columns = "2fr 1fr"),
    #   bslib$card(
    #     bslib$card_header(
    #       shiny$h3("Player Attributes")
    #     ),
    #     bslib$card_body(
    #       shiny$uiOutput(ns("playerAttributes")) |>
    #         withSpinnerCustom(height = 60)
    #     )
    #   ),
    #   bslib$card(
    #     bslib$card_header(
    #       shiny$h3("TPE Progression")
    #     ),
    #     bslib$card_body(
    #       plotly$plotlyOutput(ns("tpeProgression")) |>
    #         withSpinnerCustom(height = 60)
    #     )
    #   )
    # ),
    # bslib$card(
    #   bslib$card_header(
    #     shiny$h3("Player History")
    #   ),
    #   bslib$card_body(
    #   )
    # )
  )
}

#' @export
server <- function(id, oid = NULL, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    query <- shiny$reactive({
      if(oid |> is.null()){
        oid <- get_query_param("oid")
        
        if (is.null(oid)) {
          NULL
        } else {
          oid |>
            as.numeric()
        }
      } else {
        oid
      }
    })
    
    players <- shiny$reactive({
      shiny$req(query())
      
      getOrganizationPlayers(oid = query()) |>
        dplyr$select(
          name,
          class,
          position,
          tpe,
          tpebank,
          username,
          discord,
          bankBalance,
          nationality,
          userStatus,
          playerStatus,
          render,
          `Seasonal Training` = purchasedTPE,
          `Times Regressed` = timesregressed,
          `Player Pronouns` = pronouns,
          team,
          affiliate,
          pid
        )
    }) |> 
      shiny$bindCache(id, query(), updated()) |> 
      shiny$bindEvent(query())
    
    teamInfo <- shiny$reactive({
      shiny$req(query())
      
      getTeamInformation(oid = query()) |> 
        dplyr$arrange(affiliate)
    }) |> 
      shiny$bindCache(id, query(), updated()) |> 
      shiny$bindEvent(query())
    
    majors <- shiny$reactive({
      players() |>
        dplyr$filter(affiliate == 1)
    })
    
    minors <- shiny$reactive({
      players() |>
        dplyr$filter(affiliate == 2)
    })

    #### Output ####
    output$information <- shiny$renderUI({
      
    })
    
    output$tabs <- shiny$renderUI({
      majorName <- teamInfo() |> 
        dplyr$filter(affiliate == 1) |> 
        dplyr$pull(name)
      minorName <- teamInfo() |> 
        dplyr$filter(affiliate == 2) |> 
        dplyr$pull(name)
      
      shiny$tabsetPanel(
        shiny$tabPanel(
          title = paste(majorName, dplyr$if_else(query() < 0, "", "(Major)")),
          reactableOutput(session$ns("major"), height = 433) |> 
            withSpinnerCustom(height = 50)
        ),
        if (nrow(minors()) > 0) {
          shiny$tabPanel(
            title = paste(minorName, "(Minor)"),
            reactableOutput(session$ns("minor"), height = 433) |> 
              withSpinnerCustom(height = 50)
          )
        },
        shiny$tabPanel(
          title = "Test",
          "tesy"
        )
      )
    })
    
    output$major <- renderReactable({
      orgReactable(majors())
    })
    
    output$minor <- renderReactable({
      orgReactable(minors())
    })
    
    output$clubLogo <- shiny$renderUI({
      data <- teamInfo() |> 
        dplyr$arrange(affiliate)
      
      shiny$tagList(
        shiny$div(
          style = glue$glue(
            "display: flex; 
             flex-direction: column; 
             align-items: center; /* center horizontally */ 
             justify-content: space-between; 
             width: 150px; 
             height: {height}px; 
             margin: 0 auto;",
            height = nrow(data) * 150
            ),
          # Top-left image
          if (nrow(data) >= 1) {
            shiny$div(
              shiny$img(
                src = sprintf('static/logo/%s.png', data$name[1]),
                style = "height: 150px; padding: 2px;",
                alt = data$name[1],
                title = data$fullname[1]
              )
            )
          },
          # Bottom-right image
          if (nrow(data) >= 2) {
            shiny$div(
              shiny$img(
                src = sprintf('static/logo/%s.png', data$name[2]),
                style = "height: 150px; padding: 2px;",
                alt = data$name[2],
                title = data$fullname[2]
              )
            )
          }
        )
      )
        
      
    })
    
    output$orgInfo <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo()
      
      # Extract up to three names (already joined from allplayersview)
      managers <- c(
        paste(
          "Organizational Manager:",
          dplyr$if_else(data$om[1] |> is.na(), "None", data$om[1])
        ),
        paste(
          "Assistant Manager:",
          dplyr$if_else(data$am1[1] |> is.na(), "None", data$am1[1])
        ),
        paste(
          "Assistant Manager:",
          dplyr$if_else(data$am2[1] |> is.na(), "None", data$am2[1])
        )
      )
      
      # Build a nice list
      shiny$div(
        style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
        shiny$h4("Management", style = "margin-bottom: 6px;"),
        lapply(managers, function(name) {
          shiny$div(
            style = glue$glue(
              "padding: 4px 8px;
                background: {bg};
                border-radius: 4px;
                width: fit-content;",
              bg = data$primary[1]
            ),
            name
          )
        })
      )
    })
  #   
  #   
  #   output$playerName <- shiny$renderUI({
  #     data <- playerData()
  # 
  #     shiny$tagList(
  #       shiny$h2(paste(data$name, paste0("(", data$class, ")"), sep = " ")),
  #       shiny$h5(paste0("(", data$pronouns, ")")),
  #       shiny$h3(paste0("@", data$username))
  #     )
  #   }) 
  #   
  # 
  #   output$playerInfo <- shiny$renderUI({
  #     data <- playerData()
  # 
  #     value <-
  #       data |>
  #       dplyr$select(
  #         dplyr$contains("pos_")
  #       ) |>
  #       pivot_longer(
  #         dplyr$everything()
  #       ) |>
  #       dplyr$mutate(
  #         name = str_remove(name, pattern = "pos_") |>
  #           str_to_upper()
  #       )
  # 
  #     shiny$tagList(
  #       bslib$layout_columns(
  #         col_widths = c(6, 6),
  #         shiny$tagList(
  #           shiny$h4(paste("TPE: ", data$tpe)),
  #           shiny$h4(paste("Banked TPE: ", data$tpebank)),
  #           shiny$h4(paste("Player Status: "), 
  #                    data$playerStatus, 
  #                    class = data$playerStatus |> 
  #                      str_to_lower()),
  #           shiny$h4(paste("User Status: "), 
  #                    data$userStatus, 
  #                    class = data$userStatus |> 
  #                      str_to_lower()),
  #           shiny$h5(paste("Nationality:"), data$nationality),
  #           shiny$h5(paste("Render: "), data$render),
  #           shiny$h5(paste("Footedness "),
  #                     shiny$div(
  #                       style = "display: flex; gap: 5px; align-items: center; margin-top: 5px;",
  #                       
  #                       shiny$tags$svg(
  #                         xmlns = "http://www.w3.org/2000/svg",
  #                         shiny$tags$title(paste0("Left foot: ", data$`left foot`)),
  #                         width = "40", height = "40", viewBox = "0 0 100 100",
  #                         shiny$tags$path(
  #                           d="M 65.793945 6.763916 A 8.7670002 8.7670002 0 0 0 57.0271 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 24.298096 A 8.7670002 8.7670002 0 0 0 74.561035 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 6.763916 z M 47.711914 12.860107 A 5.4689999 5.4689999 0 0 0 42.24292 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 23.798096 A 5.4689999 5.4689999 0 0 0 53.180908 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 12.860107 z M 36.160889 22.351074 A 4.342 4.342 0 0 0 31.819092 26.693115 A 4.342 4.342 0 0 0 36.160889 31.034912 A 4.342 4.342 0 0 0 40.50293 26.693115 A 4.342 4.342 0 0 0 36.160889 22.351074 z M 55.288086 27.615967 C 43.962086 27.615967 34.781006 36.797047 34.781006 48.123047 C 34.781006 50.335047 35.141039 52.461008 35.790039 54.458008 C 36.226039 56.250008 36.92891 57.932994 37.85791 59.468994 L 37.761963 59.523926 L 52.615967 85.25293 C 54.183967 89.89293 58.562957 93.237061 63.730957 93.237061 C 70.214957 93.237061 75.474121 87.981094 75.474121 81.496094 C 75.474121 78.683094 74.482055 76.102078 72.831055 74.080078 L 72.903076 74.042969 C 71.516076 71.805969 70.700928 69.176098 70.700928 66.350098 C 70.700928 63.886098 71.317066 61.568029 72.393066 59.530029 L 72.358887 59.479004 C 74.526887 56.227004 75.794922 52.324047 75.794922 48.123047 C 75.794922 36.797047 66.613086 27.615967 55.288086 27.615967 z M 28.549072 31.542969 A 4.342 4.342 0 0 0 24.207031 35.88501 A 4.342 4.342 0 0 0 28.549072 40.227051 A 4.342 4.342 0 0 0 32.891113 35.88501 A 4.342 4.342 0 0 0 28.549072 31.542969 z ",
  #                           fill = dplyr$case_when(
  #                             data$`left foot` == 20 ~ constant$green,
  #                             data$`left foot` == 15 ~ constant$yellow,
  #                             TRUE ~ constant$red
  #                           )
  #                         )
  #                       ),
  #                       
  #                       shiny$tags$svg(
  #                         xmlns = "http://www.w3.org/2000/svg",
  #                         shiny$tags$title(paste0("Right foot: ", data$`right foot`)),
  #                         width = "40", height = "40", viewBox = "0 0 100 100",
  #                         style = "transform: scaleX(-1);",
  #                         shiny$tags$path(
  #                           d="M 65.793945 6.763916 A 8.7670002 8.7670002 0 0 0 57.0271 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 24.298096 A 8.7670002 8.7670002 0 0 0 74.561035 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 6.763916 z M 47.711914 12.860107 A 5.4689999 5.4689999 0 0 0 42.24292 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 23.798096 A 5.4689999 5.4689999 0 0 0 53.180908 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 12.860107 z M 36.160889 22.351074 A 4.342 4.342 0 0 0 31.819092 26.693115 A 4.342 4.342 0 0 0 36.160889 31.034912 A 4.342 4.342 0 0 0 40.50293 26.693115 A 4.342 4.342 0 0 0 36.160889 22.351074 z M 55.288086 27.615967 C 43.962086 27.615967 34.781006 36.797047 34.781006 48.123047 C 34.781006 50.335047 35.141039 52.461008 35.790039 54.458008 C 36.226039 56.250008 36.92891 57.932994 37.85791 59.468994 L 37.761963 59.523926 L 52.615967 85.25293 C 54.183967 89.89293 58.562957 93.237061 63.730957 93.237061 C 70.214957 93.237061 75.474121 87.981094 75.474121 81.496094 C 75.474121 78.683094 74.482055 76.102078 72.831055 74.080078 L 72.903076 74.042969 C 71.516076 71.805969 70.700928 69.176098 70.700928 66.350098 C 70.700928 63.886098 71.317066 61.568029 72.393066 59.530029 L 72.358887 59.479004 C 74.526887 56.227004 75.794922 52.324047 75.794922 48.123047 C 75.794922 36.797047 66.613086 27.615967 55.288086 27.615967 z M 28.549072 31.542969 A 4.342 4.342 0 0 0 24.207031 35.88501 A 4.342 4.342 0 0 0 28.549072 40.227051 A 4.342 4.342 0 0 0 32.891113 35.88501 A 4.342 4.342 0 0 0 28.549072 31.542969 z ",
  #                           fill = dplyr$case_when(
  #                             data$`right foot` == 20 ~ constant$green,
  #                             data$`right foot` == 15 ~ constant$yellow,
  #                             TRUE ~ constant$red
  #                           )
  #                         )
  #                       )
  #                     )
  #           )
  #         ),
  #         shiny$tagList(
  #           shiny$h4("Traits"),
  #           data$traits |>
  #             str_split(pattern = constant$traitSep) |>
  #             unlist() |>
  #             paste(collapse = "<br>") |>
  #             shiny$HTML(),
  #           shiny$br(),
  #           shiny$h4("Primary Position(s)"),
  #           value |>
  #             dplyr$filter(value == 20) |>
  #             dplyr$select(name) |>
  #             unlist() |>
  #             paste(collapse = ", ") |>
  #             shiny$HTML(),
  #           shiny$h4("Secondary Position(s)"),
  #           value |>
  #             dplyr$filter(value < 20, value >= 10) |>
  #             dplyr$select(name) |>
  #             unlist() |>
  #             paste(collapse = ", ") |>
  #             shiny$HTML(),
  #           shiny$h5("Bank balance:", paste0("$", comma(data$bankBalance)))
  #         )
  #       )
  #     )
  #   }) 
  #   
  #   output$matchStatistics <- renderReactable({
  #     data <- playerData()
  # 
  #     if (data$pos_gk == 20) {
  #       matches <-
  #         readAPI(
  #           url = "https://api.simulationsoccer.com/index/latestGames",
  #           query = list(name = data$name, outfield = FALSE)
  #         )
  #     } else {
  #       matches <-
  #         readAPI(
  #           url = "https://api.simulationsoccer.com/index/latestGames",
  #           query = list(name = data$name)
  #         )
  #     }
  # 
  #     if (!(matches |> is_empty())) {
  #       matches |>
  #         recordReactable()
  #     } else {
  #       NULL
  #     }
  #   })
  #   
  #   output$careerStatistics <- renderReactable({
  #     data <- playerData()
  #     
  #     if (data$pos_gk == 20) {
  #       matches <-
  #         getLeagueIndex(outfield = FALSE, season = "ALL", league = "ALL",
  #                        name = data$name, career = TRUE)
  #     } else {
  #       matches <-
  #         getLeagueIndex(outfield = TRUE, season = "ALL", league = "ALL",
  #                        name = data$name, career = TRUE)
  #     }
  #     
  #     if (!(matches |> is_empty())) {
  #       matches |>
  #         dplyr$relocate(season = max_season) |> 
  #         dplyr$arrange(dplyr$desc(season)) |> 
  #         dplyr$select(!name) |> 
  #         indexReactable(search = FALSE, club = TRUE)
  #     } else {
  #       NULL
  #     }
  #   }) 
  # 
  #   output$playerAttributes <- shiny$renderUI({
  #     data <- playerData()
  # 
  #     attributeReactable(data, session, output)
  #   })
  #   
  #   output$tpeProgression <- plotly$renderPlotly({
  #     tpe <- historyTPE()
  # 
  #     if (nrow(tpe) < 2) {
  #       plotly$plot_ly(mode = "markers", type = "scatter") |>
  #         plotly$add_annotations(
  #           text = "The player has had no TPE<br>progression in the Portal",
  #           x = 0.5, y = 0.5,
  #           xref = "paper", yref = "paper",
  #           showarrow = FALSE,
  #           font = list(size = 20),
  #           align = "center",
  #           borderpad = 10,
  #           bgcolor = "rgba(255, 255, 255, 0.5)"
  #         ) |>
  #         plotly$layout(
  #           xaxis = list(
  #             showgrid = FALSE,
  #             zeroline = FALSE,
  #             showline = FALSE,
  #             showticklabels = FALSE
  #           ),
  #           yaxis = list(
  #             showgrid = FALSE,
  #             zeroline = FALSE,
  #             showline = FALSE,
  #             showticklabels = FALSE
  #           ),
  #           margin = list(l = 0, r = 0, b = 0, t = 0),
  #           plot_bgcolor = "#333333", # background color
  #           paper_bgcolor = "#333333"
  #         ) |>
  #         plotly$config(
  #           displayModeBar = TRUE, # Enable display of mode bar (optional, true by default)
  #           modeBarButtonsToRemove = list(
  #             "toImage", "zoom2d", "pan2d", "select2d",
  #             "lasso2d", "zoomIn2d", "zoomOut2d",
  #             "autoScale2d", "resetScale2d"
  #           ),
  #           displaylogo = FALSE # Remove Plotly logo
  #         )
  #     } else {
  #       visData <-
  #         tpe |>
  #         dplyr$mutate(
  #           WeekStart =
  #           floor_date(
  #             Time |>
  #               as_date(),
  #             "week",
  #             week_start = 1
  #           )
  #         ) |>
  #         dplyr$group_by(WeekStart) |>
  #         dplyr$summarize(total = sum(`TPE Change`, na.rm = TRUE)) |>
  #         complete(
  #           WeekStart =
  #           seq(
  #             min(WeekStart),
  #             floor_date(
  #               today() |>
  #                 as_date(tz = "US/Pacific"),
  #               "week",
  #               week_start = 1
  #             ),
  #             by = "week"
  #           ),
  #           fill = list(total = 0)
  #         ) |>
  #         dplyr$ungroup() |>
  #         dplyr$mutate(
  #           cumulative = cumsum(total),
  #           week = seq_len(dplyr$n())
  #         ) |>
  #         suppressMessages()
  # 
  #       plotly$plot_ly(visData, hoverinfo = "text") |>
  #         plotly$add_trace(
  #           x = ~week, y = ~cumulative, type = "scatter", mode = "markers+lines",
  #           line = list(color = constant$sslGold),
  #           marker = list(size = 5, color = constant$sslGold),
  #           text = ~ paste("Week:", week, "<br>TPE:", cumulative)
  #         ) |>
  #         plotly$layout(
  #           title = list(
  #             text = "TPE Progression",
  #             font = list(color = "white") # Set title text color to white
  #           ),
  #           xaxis = list(
  #             title = "Time",
  #             tickfont = list(color = "white"), # Set x-axis tick labels color to white
  #             titlefont = list(color = "white"), # Set x-axis title color to white
  #             dtick = 1,
  #             showgrid = FALSE
  #           ),
  #           yaxis = list(
  #             title = "TPE",
  #             range = c(250, 2100),
  #             tickfont = list(color = "white"), # Set y-axis tick labels color to white
  #             titlefont = list(color = "white"), # Set y-axis title color to white
  #             dtick = 200, # Show tickmarks at intervals of 200
  #             gridcolor = "rgba(255, 255, 255, 0.5)", # Set gridline color to white with opacity
  #             gridwidth = 1 # Set gridline width
  #           ),
  #           plot_bgcolor = "#333333", # background color
  #           paper_bgcolor = "#333333", # plot area background color
  #           showlegend = FALSE # Hide legend (optional)
  #         ) |>
  #         plotly$config(
  #           displayModeBar = TRUE, # Enable display of mode bar (optional, true by default)
  #           modeBarButtonsToRemove = list(
  #             "zoom2d", "pan2d", "select2d",
  #             "lasso2d", "zoomIn2d", "zoomOut2d",
  #             "autoScale2d", "resetScale2d"
  #           ),
  #           displaylogo = FALSE # Remove Plotly logo
  #         )
  #     }
  #   })
  #   
  #   output$tpe <- renderReactable({
  #     data <- playerData()
  #     tpe <- historyTPE()
  # 
  #     if (tpe |> is_empty()) {
  #       NULL
  #     } else {
  #       tpe |>
  #         dplyr$mutate(Time = as_datetime(Time)) |>
  #         reactable(
  #           columns =
  #             list(
  #               Time = colDef(format = colFormat(datetime = TRUE))
  #             )
  #         )
  #     }
  #   }) 
  #   
  #   output$update <- renderReactable({
  #     data <- playerData()
  #     updates <- getUpdateHistory(data$pid)
  #     if (updates |> is_empty()) {
  #       NULL
  #     } else {
  #       updates |>
  #         dplyr$mutate(Time = as_datetime(Time)) |>
  #         reactable(
  #           columns =
  #             list(
  #               Time = colDef(format = colFormat(datetime = TRUE))
  #             )
  #         )
  #     }
  #   }) 
  #   
  #   output$bank <- renderReactable({
  #     data <- playerData()
  #     bank <- getBankHistory(data$pid)
  #     if (bank |> is_empty()) {
  #       NULL
  #     } else {
  #       bank |>
  #         dplyr$mutate(Time = as_datetime(Time)) |>
  #         reactable(
  #           columns =
  #             list(
  #               Time = colDef(format = colFormat(datetime = TRUE)),
  #               Transaction = colDef(
  #                 format = colFormat(
  #                   digits = 0,
  #                   separators = TRUE, 
  #                   currency = "USD"
  #                 )
  #               )
  #             )
  #         )
  #     }
  #   })
  })
}
