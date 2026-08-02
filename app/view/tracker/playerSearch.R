box::use(
  bslib,
  dplyr[arrange, desc, mutate, rename_with, select],
  reactable[
    colDef, 
    colFormat, 
    reactable, 
    reactableLang,
    reactableOutput, 
    renderReactable
  ],
  shiny,
  shiny.router[route_link],
  stringr[str_to_upper],
)

box::use(
  app/logic/constant,
  app/logic/get/getPlayer[getPlayers],
  app/logic/ui/reactableHelper[linkOrganization],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$layout_column_wrap(
      width = NULL,
      style = bslib$css(grid_template_columns = "2fr 3fr"),
      shiny$h2("Player Search"),
      shiny$radioButtons(
        ns("retired"),
        "Include Retired?",
        choices = c("Yes", "No"),
        selected = "No",
        inline = TRUE
      )
    ),
    reactableOutput(ns("players"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    output$players <- renderReactable({
      data <- getPlayers(active = (input$retired == "No")) |>
        select(name, username, pid, team, class, position, tpe, 
               tpebank, nationality, bankBalance, playerStatus, 
               userStatus) |> 
        arrange(desc(tpe)) |> 
        mutate(searchName = iconv(name, from = "UTF-8", , to = "ASCII//TRANSLIT"))

      data |>
        rename_with(str_to_upper) |> 
        reactable(
          searchable = TRUE,
          language = reactableLang(
            searchPlaceholder = "Search/filter for player, username, team or nationality"
          ),
          defaultPageSize = 25,
          showPageSizeOptions = TRUE,
          defaultColDef = colDef(searchable = FALSE),
          columns = list(
            SEARCHNAME = colDef(
              searchable = TRUE,
              show = FALSE
            ),
            USERNAME = colDef(
              searchable = TRUE
            ),
            NAME = colDef(
              searchable = TRUE,
              cell = function(value, rowIndex) {
                pid <- data[rowIndex, "pid"] # Get the corresponding pid
                shiny$a(
                  href = route_link(paste0("tracker/player?pid=", pid)),
                  value # Display the name as the link text
                )
              }
            ),
            TEAM = colDef(
              width = 200, 
              align = "left",
              searchable = TRUE,
              cell = function(value) {
                linkOrganization(value)
              }
            ),
            NATIONALITY = colDef(
              searchable = TRUE,
              cell = function(value) {
                shiny$div(
                  class = "flex-row flex-center",
                  shiny$img(
                    style = "
                      height: 1em;
                      width: 2em;
                      padding: 0px 5px;
                    ",
                    src = sprintf("https://flagcdn.com/%s.svg", constant$nationsTwoLetter[value] |> tolower()),
                    alt = value,
                    title = value
                  ),
                  shiny$p(
                    style = "margin: auto;",
                    value
                  )
                )
              }
            ),
            BANKBALANCE = colDef(
              width = 120, 
              format = colFormat(
                digits = 0,
                separators = TRUE,
                currency = "USD"
              )
            ),
            PLAYERSTATUS = 
              colDef(
                cell = function(value) {
                  shiny$div(
                    class = value |> tolower(),
                    value
                  )
                }
              ),
            USERSTATUS = 
              colDef(
                cell = function(value) {
                  shiny$div(
                    class = value |> tolower(),
                    value
                  )
                }
              ),
            PID = colDef(show = FALSE)
          ),
          rowStyle = function(index) {
            if (data[index, "team"] == "Retired") {
              list(fontStyle = "italic")
            }
          }
        )
    })
    
  })
}
