box::use(
  bslib,
  dplyr[arrange, desc, rename_with, select],
  reactable[
    colDef, 
    colFormat, 
    reactable, 
    reactableOutput, 
    renderReactable
  ],
  shiny,
  shiny.router[route_link],
  stringr[str_to_upper],
)

box::use(
  app / logic / db / get[getPlayers],
  app / logic / ui / reactableHelper[linkOrganization],
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
        arrange(desc(tpe))

      data |>
        rename_with(str_to_upper) |> 
        reactable(
          searchable = TRUE,
          defaultPageSize = 25,
          showPageSizeOptions = TRUE,
          defaultColDef = colDef(searchable = FALSE),
          columns = list(
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
              cell = function(value) {
                linkOrganization(value)
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
