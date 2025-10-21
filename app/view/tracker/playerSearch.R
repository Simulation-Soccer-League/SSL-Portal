box::use(
  dplyr[rename_with, select],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  shiny,
  shiny.router[route_link],
  stringr[str_to_upper],
)

box::use(
  app / logic / db / get[getPlayers],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$h2("Player Search"),
    reactableOutput(ns("players"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    output$players <- renderReactable({
      data <- getPlayers(active = TRUE) |>
        select(name, username, pid, team, position, tpe, 
               tpebank, class, playerStatus, userStatus)

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
            PID = colDef(show = FALSE)
          )
        )
    })
  })
}
