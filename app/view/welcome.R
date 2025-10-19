box::use(
  bslib,
  dplyr[mutate, rename_with, select],
  plotly[config, layout, plot_ly, plotlyOutput, renderPlotly],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  shiny,
  shiny.router[route_link],
  stringr[str_to_upper,],
  tippy[tippy],
)

box::use(
  app / logic / constant,
  app / logic / db / api[readAPI],
  app / logic / db / get[getRecentCreates, getSchedule, getStandings, getTopEarners],
  app / logic / ui / cards[resultCard],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$uiOutput(ns("information")),
    bslib$card(
      bslib$card_header(
        flexRow(
          shiny$tagList(
            shiny$tags$style("align-items: center; justify-content: space-between;"),
            shiny$div(style = "width: 150px;", class = "hide-in-mobile"),
            shiny$div(shiny$h5("Latest Results"), style = "width: 100%;"),
            shiny$div(
              shiny$div(
                shiny$selectInput(
                  inputId = ns("selectedLeague"),
                  label = NULL,
                  choices =
                    c(
                      "All Leagues" = "ALL",
                      "Major" = "1",
                      "Minor" = "2",
                      "Cup" = "0"
                    ),
                  width = "150px"
                )
              ),
              style = "font-size: 14px; font-weight: 400;"
            )
          )
        ),
        bslib$card_body(
          shiny$uiOutput(ns("schedule")) |>
            withSpinnerCustom(height = 20)
        )
      ),
      min_height = "200px"
    ),
    bslib$layout_column_wrap(
      width = 1 / 2,
      bslib$card(
        bslib$card_header(
          shiny$h5("Major League Standings")
        ),
        bslib$card_body(
          reactableOutput(ns("standings_1")) |>
            withSpinnerCustom(height = 20)
        )
      ),
      bslib$card(
        bslib$card_header(
          shiny$h5("Minor League Standings")
        ),
        bslib$card_body(
          reactableOutput(ns("standings_2")) |>
            withSpinnerCustom(height = 20)
        )
      ),
      bslib$card(
        bslib$card_header(
          shiny$h4("Weekly top earners")
        ),
        bslib$card_body(
          reactableOutput(ns("weeklyLeaders")) |>
            withSpinnerCustom(height = 40)
        )
      ),
      bslib$card(
        bslib$card_header(
          shiny$h4("Recent creates")
        ),
        bslib$card_body(
          reactableOutput(ns("created")) |>
            withSpinnerCustom(height = 40)
        )
      )
    ),
    bslib$card(
      bslib$card_header(
        shiny$h4("Activity Checks")
      ),
      bslib$card_body(
        plotlyOutput(ns("activityChecks")) |>
          withSpinnerCustom(height = 40) |>
          shiny$div(class = "plotlyBorder")
      )
    )
  )
}

#' @export
server <- function(id, usergroup) {
  shiny$moduleServer(
    id,
    function(input, output, session) {
      #### INFORMATION ####
      output$information <- shiny$renderUI({
        if (any(5 %in% usergroup)) {
          bslib$card(
            bslib$card_header(
              "Information"
            ),
            bslib$card_body(
              shiny$h2("Your account needs to be activated in order to access the rest of the portal functions. Please check the e-mail used when registering on the SSL forums.") |> # nolint: line_length_linter
                shiny$div(class = "Retired")
            )
          )
        }
      })

      #### Latest league standings ####
      lapply(1:2,
        FUN = function(division) {
          output[[paste0("standings_", division)]] <- renderReactable({
            standings <- getStandings(league = division, season = constant$currentSeason$season)

            if (!(standings |> is_empty())) {
              standings |>
                select(
                  Team,
                  Wins:Losses,
                  Points
                ) |>
                reactable(
                  defaultColDef = colDef(minWidth = 30),
                  pagination = FALSE,
                  columns =
                    list(
                      Team = colDef(
                        minWidth = 100,
                        cell = function(value) {
                          image <- shiny$img(
                            src = sprintf("static/logo/%s (Custom).png", value),
                            style = "height: 25px;",
                            alt = value, 
                            title = value
                          )
                          list <-
                            shiny$tagList(
                              flexRow(style = "align-items: center; gap: 8px;", shiny$tagList(
                                image,
                                shiny$span(class = "truncated-text", value)
                              ))
                            )
                        }
                      ),
                      Wins = colDef(
                        header = tippy(
                          "W",
                          "Wins",
                          placement = "top",
                          theme = "ssl",
                          arrow = TRUE
                        )
                      ),
                      Draws = colDef(
                        header = tippy(
                          "D",
                          "Draws",
                          placement = "top",
                          theme = "ssl",
                          arrow = TRUE
                        )
                      ),
                      Losses = colDef(
                        header = tippy(
                          "L",
                          "Losses",
                          placement = "top",
                          theme = "ssl",
                          arrow = TRUE
                        )
                      ),
                      Points = colDef(
                        header = tippy(
                          "P",
                          "Points",
                          placement = "top",
                          theme = "ssl",
                          arrow = TRUE
                        )
                      )
                    )
                )
            }
          })
        }
      )

      #### Latest results ####
      schedule <- shiny$reactive({
        shiny$req(input$selectedLeague)

        league <- input$selectedLeague

        getSchedule(league = league, season = constant$currentSeason$season)
      })

      output$schedule <- shiny$renderUI({
        league <- input$selectedLeague

        data <- schedule()

        if (data |> is_empty()) {
          "No schedule is available yet"
        } else {
          shiny$tagList(
            shiny$div(
              class = "results",
              id = "results-scroll",
              lapply(
                seq_len(nrow(data)),
                function(i) {
                  resultCard(data, i)
                }
              )
            ),
            shiny$tags$script(
              shiny$HTML("
                  $(document).ready(function() {
                    var div = document.getElementById('results-scroll');
                    var width = 0;
                    for (var i = 0; i < div.children.length; i++) {
                      var score = $(div.children[i]).find('h4').text().trim();
                      if (!score.match(/^\\d+-\\d+$/)) {
                        width = div.children[i].clientWidth * (i-6);
                        break;
                      } else {
                        width = div.children[i].clientWidth * i
                      }
                    }
                    div.scrollLeft = width;
                  });
                ")
            )
          )
        }
      }) |>
        shiny$bindCache(input$selectedLeague)

      #### Weekly TPE Leaders ####
      output$weeklyLeaders <- renderReactable({
        data <- getTopEarners() 
        
        data |>
          rename_with(str_to_upper) |> 
          reactable(
            defaultColDef = colDef(minWidth = 75),
            columns = list(
              PID = colDef(show = FALSE),
              NAME = colDef(
                searchable = TRUE,
                cell = function(value, rowIndex) {
                  pid <- data[rowIndex, "pid"] # Get the corresponding pid
                  shiny$a(
                    href = route_link(paste0("tracker/player?pid=", pid)),
                    value # Display the name as the link text
                  )
                }
              )
            )
          )
      })

      #### Recently created ####
      output$created <- renderReactable({
        data <- getRecentCreates() 
        
        data |>
          rename_with(str_to_upper) |> 
          reactable(
            defaultColDef = colDef(minWidth = 25),
            columns = list(
              PID = colDef(show = FALSE),
              NAME = colDef(
                searchable = TRUE,
                cell = function(value, rowIndex) {
                  pid <- data[rowIndex, "pid"] # Get the corresponding pid
                  shiny$a(
                    href = route_link(paste0("tracker/player?pid=", pid)),
                    value # Display the name as the link text
                  )
                }
              )
            )
          )
      })

      output$activityChecks <- renderPlotly({
        readAPI("https://api.simulationsoccer.com/player/acHistory") |>
          mutate(weekYear = paste(paste0("W", nweeks))) |>
          plot_ly(
            x = ~weekYear, y = ~count, type = "scatter", mode = "lines+markers",
            hoverinfo = "text",
            line = list(color = constant$sslGold),
            marker = list(size = 5, color = constant$sslGold),
            text = ~ paste("#AC: ", count)
          ) |>
          layout(
            xaxis = list(
              title = "Time",
              tickfont = list(color = "white"), # Set x-axis tick labels color to white
              titlefont = list(color = "white"), # Set x-axis title color to white
              dtick = 2,
              gridwidth = 0.5, 
              gridcolor = "rgba(64, 64, 64, 0.5)"
            ),
            yaxis = list(
              title = "#ACs",
              # range = c(0, 220),
              tickfont = list(color = "white"), # Set y-axis tick labels color to white
              titlefont = list(color = "white"), # Set y-axis title color to white
              dtick = 20, # Show tickmarks at intervals of 200
              gridcolor = "rgba(255, 255, 255, 0.5)", # Set gridline color to white with opacity
              gridwidth = 1 # Set gridline width
            ),
            plot_bgcolor = "#333333", # background color
            paper_bgcolor = "#333333", # plot area background color
            showlegend = FALSE # Hide legend (optional)
          ) |>
          config(
            displayModeBar = TRUE, # Enable display of mode bar (optional, true by default)
            modeBarButtonsToRemove = list(
              "zoom2d", "pan2d", "select2d",
              "lasso2d", "zoomIn2d", "zoomOut2d",
              "autoScale2d", "resetScale2d"
            ),
            displaylogo = FALSE # Remove Plotly logo
          )
      })
    }
  )
}
