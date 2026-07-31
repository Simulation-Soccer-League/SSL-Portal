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
  shiny.router[get_query_param],
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
  app/logic/get/getPlayer[
    getBankHistory,
    getPlayer,
    getPlayerAttributes,
    getTpeHistory, 
    getUpdateHistory
  ],
  app/logic/get/getIndex[
    getLatestGames,
    getLeagueIndex,
  ],
  app/logic/ui/reactableHelper[
    indexReactable,
    linkOrganization,
    recordReactable,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$layout_column_wrap(
      width = 1 / 2,
      heights_equal = "row",
      bslib$card(
        bslib$card_header(
          shiny$uiOutput(ns("playerHeader")) |> 
            withSpinnerCustom(height = 100)
        ),
        bslib$card_body(
          shiny$uiOutput(ns("playerInfo")) |>
            withSpinnerCustom(height = 400)
        )
      ),
      bslib$card(
        bslib$card_header(
          shiny$tabsetPanel(
            shiny$tabPanel(
              title = "Last 10 games",
              reactableOutput(ns("matchStatistics")) |> 
                withSpinnerCustom(height = "400px")
            ),
            shiny$tabPanel(
              title = "Career Statistics",
              reactableOutput(ns("careerStatistics")) |> 
                withSpinnerCustom(height = "400px")
            )
          )
        )
      )
    ),
    bslib$layout_column_wrap(
      width = NULL,
      style = bslib$css(grid_template_columns = "2fr 1fr"),
      bslib$card(
        bslib$card_header(
          shiny$h3("Player Attributes")
        ),
        bslib$card_body(
          shiny$uiOutput(ns("playerAttributes")) |>
            withSpinnerCustom(height = 60)
        )
      ),
      bslib$card(
        bslib$card_header(
          shiny$h3("TPE Progression")
        ),
        bslib$card_body(
          plotly$plotlyOutput(ns("tpeProgression")) |>
            withSpinnerCustom(height = 60)
        )
      )
    ),
    bslib$card(
      bslib$card_header(
        shiny$h3("Player History")
      ),
      bslib$card_body(
        shiny$tabsetPanel(
          shiny$tabPanel(
            title = "TPE History",
            reactableOutput(ns("tpe"), height = 450)
          ),
          shiny$tabPanel(
            title = "Update History",
            reactableOutput(ns("update"), height = 450)
          ),
          shiny$tabPanel(
            title = "Bank History",
            reactableOutput(ns("bank"), height = 450)
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, pid = NULL, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    query <- shiny$reactive({
      if (pid |> is.null()) {
        pid <- get_query_param("pid")
        
        if (is.null(pid)) {
          NULL
        } else {
          pid |>
            as.numeric()
        }
      } else {
        pid
      }
    })

    playerData <- shiny$reactive({
      shiny$req(query())

      getPlayer(query())
    }) |> 
      shiny$bindEvent(query(), updated())
    
    historyTPE <- shiny$reactive({
      shiny$req(query())

      getTpeHistory(query())
    }) |> 
      shiny$bindEvent(query(), updated())

    fillColor <- function(value) {
      dplyr$case_when(
        value == 20 ~ constant$green,
        value >= 15 ~ constant$yellow,
        TRUE ~ "#ccc"
      )
    }
    
    #### Output ####
    output$playerHeader <- shiny$renderUI({
      data <- playerData()
      
      shiny$div(
        class = "flex-row flex-center",
        style = "text-align: left;",
        shiny$div(
          shiny$h2(
            sprintf("%s (%s)", data$name, data$class)
          ),
          shiny$div(
            class = "flex-row flex-baseline",
            shiny$em(
              style = "font-size: 1.2em;",
              sprintf("@%s", data$username)
            ),
            shiny$div(
              class = "flex-row flex-center",
              shiny$img(
                style = "
                    height: 1em;
                    width: 2em;
                    padding: 0px 5px;
                  ",
                src = sprintf("https://flagcdn.com/%s.svg", constant$nationsTwoLetter[data$nationality] |> tolower()),
                alt = data$nationality,
                title = data$nationality
              ),
              shiny$p(
                style = "margin: auto;",
                data$nationality
              )
            )
            
          )
        ),
        linkOrganization(value = data$team, onlyImg = TRUE, height = 100)
      )
    })
    
    output$playerInfo <- shiny$renderUI({
      data <- playerData()
      
      posValue <-
        data |>
        dplyr$select(
          dplyr$contains("pos_")
        ) |>
        pivot_longer(
          dplyr$everything()
        ) |>
        dplyr$mutate(
          name = str_remove(name, pattern = "pos_") |>
            str_to_upper()
        )
      
      shiny$div(
        class = "flex-row",
        shiny$div(
          class = "flex-column",
          shiny$p("TPE: ", data$tpe),
          shiny$p("Banked TPE: ", data$tpebank),
          shiny$p(shiny$span(class = data$playerStatus |> tolower(), data$playerStatus), " player"),
          shiny$p(shiny$span(class = data$userStatus |> tolower(), data$userStatus), " user"),
          shiny$p(tippy("Render:  ", "The player likeness", theme = "ssl"), data$render),
          shiny$div(
            class = "flex-column", 
            shiny$p("Traits: "),
            data$traits |>
              str_split(pattern = constant$traitSep) |>
              unlist() |>
              paste(collapse = "<br>") |>
              shiny$HTML(),
          ),
          shiny$div(
            style = "display: flex; gap: 5px; align-items: center; margin-top: 5px;",
            shiny$tags$svg(
              xmlns = "http://www.w3.org/2000/svg",
              shiny$tags$title(paste0("Left foot: ", data$`left foot`)),
              width = "40", height = "40", viewBox = "0 0 100 100",
              shiny$tags$path(
                d = "M 65.793945 6.763916 A 8.7670002 8.7670002 0 0 0 57.0271 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 24.298096 A 8.7670002 8.7670002 0 0 0 74.561035 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 6.763916 z M 47.711914 12.860107 A 5.4689999 5.4689999 0 0 0 42.24292 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 23.798096 A 5.4689999 5.4689999 0 0 0 53.180908 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 12.860107 z M 36.160889 22.351074 A 4.342 4.342 0 0 0 31.819092 26.693115 A 4.342 4.342 0 0 0 36.160889 31.034912 A 4.342 4.342 0 0 0 40.50293 26.693115 A 4.342 4.342 0 0 0 36.160889 22.351074 z M 55.288086 27.615967 C 43.962086 27.615967 34.781006 36.797047 34.781006 48.123047 C 34.781006 50.335047 35.141039 52.461008 35.790039 54.458008 C 36.226039 56.250008 36.92891 57.932994 37.85791 59.468994 L 37.761963 59.523926 L 52.615967 85.25293 C 54.183967 89.89293 58.562957 93.237061 63.730957 93.237061 C 70.214957 93.237061 75.474121 87.981094 75.474121 81.496094 C 75.474121 78.683094 74.482055 76.102078 72.831055 74.080078 L 72.903076 74.042969 C 71.516076 71.805969 70.700928 69.176098 70.700928 66.350098 C 70.700928 63.886098 71.317066 61.568029 72.393066 59.530029 L 72.358887 59.479004 C 74.526887 56.227004 75.794922 52.324047 75.794922 48.123047 C 75.794922 36.797047 66.613086 27.615967 55.288086 27.615967 z M 28.549072 31.542969 A 4.342 4.342 0 0 0 24.207031 35.88501 A 4.342 4.342 0 0 0 28.549072 40.227051 A 4.342 4.342 0 0 0 32.891113 35.88501 A 4.342 4.342 0 0 0 28.549072 31.542969 z ",
                fill = dplyr$case_when(
                  data$`left foot` >= 19 ~ constant$green,
                  data$`left foot` == 15 ~ constant$yellow,
                  TRUE ~ constant$red
                )
              )
            ),
            shiny$tags$svg(
              xmlns = "http://www.w3.org/2000/svg",
              shiny$tags$title(paste0("Right foot: ", data$`right foot`)),
              width = "40", height = "40", viewBox = "0 0 100 100",
              style = "transform: scaleX(-1);",
              shiny$tags$path(
                d = "M 65.793945 6.763916 A 8.7670002 8.7670002 0 0 0 57.0271 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 24.298096 A 8.7670002 8.7670002 0 0 0 74.561035 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 6.763916 z M 47.711914 12.860107 A 5.4689999 5.4689999 0 0 0 42.24292 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 23.798096 A 5.4689999 5.4689999 0 0 0 53.180908 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 12.860107 z M 36.160889 22.351074 A 4.342 4.342 0 0 0 31.819092 26.693115 A 4.342 4.342 0 0 0 36.160889 31.034912 A 4.342 4.342 0 0 0 40.50293 26.693115 A 4.342 4.342 0 0 0 36.160889 22.351074 z M 55.288086 27.615967 C 43.962086 27.615967 34.781006 36.797047 34.781006 48.123047 C 34.781006 50.335047 35.141039 52.461008 35.790039 54.458008 C 36.226039 56.250008 36.92891 57.932994 37.85791 59.468994 L 37.761963 59.523926 L 52.615967 85.25293 C 54.183967 89.89293 58.562957 93.237061 63.730957 93.237061 C 70.214957 93.237061 75.474121 87.981094 75.474121 81.496094 C 75.474121 78.683094 74.482055 76.102078 72.831055 74.080078 L 72.903076 74.042969 C 71.516076 71.805969 70.700928 69.176098 70.700928 66.350098 C 70.700928 63.886098 71.317066 61.568029 72.393066 59.530029 L 72.358887 59.479004 C 74.526887 56.227004 75.794922 52.324047 75.794922 48.123047 C 75.794922 36.797047 66.613086 27.615967 55.288086 27.615967 z M 28.549072 31.542969 A 4.342 4.342 0 0 0 24.207031 35.88501 A 4.342 4.342 0 0 0 28.549072 40.227051 A 4.342 4.342 0 0 0 32.891113 35.88501 A 4.342 4.342 0 0 0 28.549072 31.542969 z ",
                fill = dplyr$case_when(
                  data$`right foot` >= 19 ~ constant$green,
                  data$`right foot` == 15 ~ constant$yellow,
                  TRUE ~ constant$red
                )
              )
            )
          )
        ),
        shiny$div(
          class = "flex-column",
          shiny$tags$svg(
            viewBox = "0 0 400 600",
            width = "80%",
            height = "100%",
            xmlns = "http://www.w3.org/2000/svg",
            shiny$tags$rect(x = 0, y = 0, width = "100%", height = "100%", fill = "#889e7a"),
            shiny$tags$g(stroke = "white", fill = "none",
              shiny$tags$rect(x = "2.5%", y = "2.5%", width = "95%", height = "95%"),
              shiny$tags$line(x1 = "2.5%", y1 = "50%", x2 = "97.5%", y2 = "50%"),
              shiny$tags$circle(cx = "50%", cy = "50%", r = "7%"),
              shiny$tags$rect(x = "12.5%", y = "2.5%", width = "75%", height = "20%"),
              shiny$tags$rect(x = "25%", y = "2.5%", width = "50%", height = "10%"),
              shiny$tags$rect(x = "12.5%", y = "77.5%", width = "75%", height = "20%"),
              shiny$tags$rect(x = "25%", y = "87.5%", width = "50%", height = "10%")
            ),
            shiny$tags$g(
              stroke = "black",
              shiny$tags$circle(
                shiny$tags$title(sprintf("Striker: %s", data$pos_st)), 
                cx = "50%", cy = "10%", r = "4%", 
                fill = fillColor(data$pos_st)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Left Attacking Midfielder: %s", data$pos_lam)), 
                cx = "20%", cy = "25%", r = "4%", 
                fill = fillColor(data$pos_lam)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Central Attacking Midfielder: %s", data$pos_cam)),
                cx = "50%", cy = "25%", r = "4%", 
                fill = fillColor(data$pos_cam)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Right Attacking Midfielder: %s", data$pos_ram)), 
                cx = "80%", cy = "25%", r = "4%", 
                fill = fillColor(data$pos_ram)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Left Midfielder: %s", data$pos_lm)), 
                cx = "20%", cy = "42.5%", r = "4%", 
                fill = fillColor(data$pos_lm)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Central Midfielder: %s", data$pos_cm)), 
                cx = "50%", cy = "42.5%", r = "4%", 
                fill = fillColor(data$pos_cm)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Right Midfielder: %s", data$pos_rm)), 
                cx = "80%", cy = "42.5%", r = "4%", 
                fill = fillColor(data$pos_rm)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Left Wingback: %s", data$pos_lwb)), 
                cx = "20%", cy = "60%", r = "4%",
                fill = fillColor(data$pos_lwb)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Central Defensive Midfielder: %s", data$pos_cdm)), 
                cx = "50%", cy = "60%", r = "4%", 
                fill = fillColor(data$pos_cdm)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Right Wingback: %s", data$pos_rwb)), 
                cx = "80%", cy = "60%", r = "4%",
                fill = fillColor(data$pos_rwb)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Left Defender: %s", data$pos_ld)), 
                cx = "20%", cy = "75%", r = "4%",
                fill = fillColor(data$pos_ld)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Central Defender: %s", data$pos_cd)), 
                cx = "50%", cy = "75%", r = "4%",
                fill = fillColor(data$pos_cd)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Right Defender: %s", data$pos_rd)), 
                cx = "80%", cy = "75%", r = "4%", 
                fill = fillColor(data$pos_rd)
              ),
              shiny$tags$circle(
                shiny$tags$title(sprintf("Goalkeeper: %s", data$pos_gk)), 
                cx = "50%", cy = "90%", r = "4%",
                fill = fillColor(data$pos_gk)
              )
            )
          )
        )
      )
      #     ),
      #     shiny$tagList(
      #       shiny$h4("Traits"),
      #       shiny$br(),
      #       shiny$h4("Primary Position(s)"),
      #       value |>
      #         dplyr$filter(value == 20) |>
      #         dplyr$select(name) |>
      #         unlist() |>
      #         paste(collapse = ", ") |>
      #         shiny$HTML(),
      #       shiny$h4("Secondary Position(s)"),
      #       value |>
      #         dplyr$filter(value < 20, value >= 10) |>
      #         dplyr$select(name) |>
      #         unlist() |>
      #         paste(collapse = ", ") |>
      #         shiny$HTML(),
      #       shiny$h5("Bank balance:", paste0("$", comma(data$bankBalance)))
      #     )
      #   )
      # )
    }) 
    
    output$matchStatistics <- renderReactable({
      data <- playerData()

      if (data$pos_gk == 20) {
        matches <-
          getLatestGames(name = data$name, outfield = FALSE)
      } else {
        matches <-
          getLatestGames(name = data$name)
      }

      if (!(matches |> is_empty())) {
        matches |>
          recordReactable()
      } else {
        NULL
      }
    })
    
    output$careerStatistics <- renderReactable({
      data <- playerData()
      
      if (data$pos_gk == 20) {
        matches <-
          getLeagueIndex(outfield = FALSE, season = "ALL", league = "ALL",
                         name = data$name, career = TRUE)
      } else {
        matches <-
          getLeagueIndex(outfield = TRUE, season = "ALL", league = "ALL",
                         name = data$name, career = TRUE)
      }
      
      if (!(matches |> is_empty())) {
        matches |>
          dplyr$relocate(season = max_season) |> 
          dplyr$arrange(dplyr$desc(season)) |> 
          dplyr$select(!name) |> 
          indexReactable(search = FALSE, club = TRUE)
      } else {
        NULL
      }
    }) 

    output$playerAttributes <- shiny$renderUI({
      data <- playerData()
      
      attrs <- getPlayerAttributes(data$pid, updated)

      groups <- attrs$group |> unique()
      
      shiny$div(
        style = "
          display: flex;
          flex-direction: row;
        ",
        purrr$map(
          .x = seq_len(groups |> length()),
          .f = function(groupIndex) {
            groupData <- attrs |> 
              dplyr$filter(group == groups[groupIndex])
            
            attributes <- groupData$attribute
            
            shiny$div(
              style = "padding: 0px 5px;",
              shiny$h4(groups[groupIndex]),
              purrr$map(
                .x = seq_len(attributes |> length()),
                .f = function(attIndex) {
                  attData <- groupData |> 
                    dplyr$filter(attribute == attributes[attIndex])
                  
                  shiny$div(
                    style = "
                    display: flex;
                    flex-direction: row;
                    align-items: center;
                    justify-content: space-between;
                    padding: 0 5px;
                    margin: 5px 0px;
                    border-radius: 8px;
                    background: var(--bottom-background);
                    box-shadow: 2px 2px 4px var(--middle-background);
                  ",
                    shiny$div(
                      style = "width: 40%; text-overflow: ellipsis; overflow: hidden; white-space: nowrap;",
                      tippy(
                        attData$attribute,
                        sprintf("%s<br/>%s", attData$attribute, attData$explanation),
                        theme = "ssl"
                      )
                    ),
                    shiny$div(
                      style = "width: 50%;",
                      shiny$tags$svg(
                        style = "width: 100%; height: 10px; display: block; margin: 7.5px 0px;",
                        shiny$tags$rect(
                          x = 0, y = 0, 
                          width = sprintf("%s%%", attData$value / 20 * 100), 
                          height = 15,
                          fill = dplyr$case_when(
                            attData$valuefill == 1 ~ constant$blue,
                            attData$valuefill == 2 ~ constant$green,
                            attData$valuefill == 3 ~ constant$yellow,
                            TRUE ~ constant$red
                          )
                        )
                      )  
                    ),
                    shiny$div(
                      style = "width: 10%; text-align: right;",
                      attData$value
                    )
                  )
                }
              )
            )
          }
        )
      )
    })
    
    output$tpeProgression <- plotly$renderPlotly({
      tpe <- historyTPE()

      if (nrow(tpe) < 2) {
        plotly$plot_ly(mode = "markers", type = "scatter") |>
          plotly$add_annotations(
            text = "The player has had no TPE<br>progression in the Portal",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 20),
            align = "center",
            borderpad = 10,
            bgcolor = "rgba(255, 255, 255, 0.5)"
          ) |>
          plotly$layout(
            xaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE
            ),
            margin = list(l = 0, r = 0, b = 0, t = 0),
            plot_bgcolor = "#333333", # background color
            paper_bgcolor = "#333333"
          ) |>
          plotly$config(
            displayModeBar = TRUE, # Enable display of mode bar (optional, true by default)
            modeBarButtonsToRemove = list(
              "toImage", "zoom2d", "pan2d", "select2d",
              "lasso2d", "zoomIn2d", "zoomOut2d",
              "autoScale2d", "resetScale2d"
            ),
            displaylogo = FALSE # Remove Plotly logo
          )
      } else {
        visData <-
          tpe |>
          dplyr$mutate(
            WeekStart =
            floor_date(
              Time |>
                as_date(),
              "week",
              week_start = 1
            )
          ) |>
          dplyr$group_by(WeekStart) |>
          dplyr$summarize(total = sum(`TPE Change`, na.rm = TRUE)) |>
          complete(
            WeekStart =
            seq(
              min(WeekStart),
              floor_date(
                today() |>
                  as_date(tz = "US/Pacific"),
                "week",
                week_start = 1
              ),
              by = "week"
            ),
            fill = list(total = 0)
          ) |>
          dplyr$ungroup() |>
          dplyr$mutate(
            cumulative = cumsum(total),
            week = seq_len(dplyr$n())
          ) |>
          suppressMessages()

        plotly$plot_ly(visData, hoverinfo = "text") |>
          plotly$add_trace(
            x = ~week, y = ~cumulative, type = "scatter", mode = "markers+lines",
            line = list(color = constant$sslGold),
            marker = list(size = 5, color = constant$sslGold),
            text = ~ paste("Week:", week, "<br>TPE:", cumulative)
          ) |>
          plotly$layout(
            title = list(
              text = "TPE Progression",
              font = list(color = "white") # Set title text color to white
            ),
            xaxis = list(
              title = "Time",
              tickfont = list(color = "white"), # Set x-axis tick labels color to white
              titlefont = list(color = "white"), # Set x-axis title color to white
              dtick = 1,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "TPE",
              range = c(250, 2100),
              tickfont = list(color = "white"), # Set y-axis tick labels color to white
              titlefont = list(color = "white"), # Set y-axis title color to white
              dtick = 200, # Show tickmarks at intervals of 200
              gridcolor = "rgba(255, 255, 255, 0.5)", # Set gridline color to white with opacity
              gridwidth = 1 # Set gridline width
            ),
            plot_bgcolor = "#333333", # background color
            paper_bgcolor = "#333333", # plot area background color
            showlegend = FALSE # Hide legend (optional)
          ) |>
          plotly$config(
            displayModeBar = TRUE, # Enable display of mode bar (optional, true by default)
            modeBarButtonsToRemove = list(
              "zoom2d", "pan2d", "select2d",
              "lasso2d", "zoomIn2d", "zoomOut2d",
              "autoScale2d", "resetScale2d"
            ),
            displaylogo = FALSE # Remove Plotly logo
          )
      }
    })
    
    output$tpe <- renderReactable({
      data <- playerData()
      tpe <- historyTPE()

      if (tpe |> is_empty()) {
        NULL
      } else {
        tpe |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
      }
    }) 
    
    output$update <- renderReactable({
      data <- playerData()
      updates <- getUpdateHistory(data$pid)
      if (updates |> is_empty()) {
        NULL
      } else {
        updates |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE))
              )
          )
      }
    }) 
    
    output$bank <- renderReactable({
      data <- playerData()
      bank <- getBankHistory(data$pid)
      if (bank |> is_empty()) {
        NULL
      } else {
        bank |>
          dplyr$mutate(Time = as_datetime(Time)) |>
          reactable(
            columns =
              list(
                Time = colDef(format = colFormat(datetime = TRUE)),
                Transaction = colDef(
                  format = colFormat(
                    digits = 0,
                    separators = TRUE, 
                    currency = "USD"
                  )
                )
              )
          )
      }
    })
  })
}
