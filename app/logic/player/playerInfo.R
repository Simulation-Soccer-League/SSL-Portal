box::use(
  bslib,
  dplyr,
  shiny,
  stringr,
  tidyr,
)

box::use(
  app/logic/constant,
  app/logic/ui/spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$card(
    bslib$card_header(
      shiny$h3("Profile Information")
    ),
    bslib$card_body(
      bslib$layout_column_wrap(
        width = NULL,
        style = bslib$css(grid_template_columns = "3fr 1fr"),
        shiny$uiOutput(ns("playerName")) |>
          withSpinnerCustom(height = 20),
        shiny$uiOutput(ns("clubLogo"), height = NULL) |>
          withSpinnerCustom(height = 20)
      ),
      shiny$uiOutput(ns("playerInfo")) |>
        withSpinnerCustom(height = 40)
    )
  )
}

#' @export
server <- function(id, updated, playerData) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Name & handle
    output$playerName <- shiny$renderUI({
      data <- playerData()
      shiny$tagList(
        shiny$h2(paste(data$name, sprintf("(%s)", data$class))),
        shiny$h4(paste0("(", data$pronouns, ")")),
        shiny$h3(paste0("@", data$username))
      )
    }) |> shiny$bindCache(updated())
    
    ## Club logo
    output$clubLogo <- shiny$renderUI({
      data <- playerData()
      shiny$img(
        src   = sprintf("static/logo/%s.png", data$team),
        style = "height:100px", alt = data$team, title = data$team
      )
    }) |> shiny$bindCache(updated())
    
    ## Detailed info
    output$playerInfo <- shiny$renderUI({
      data <- playerData()
      posTbl <- data |>
        dplyr$select(dplyr$starts_with("pos_")) |>
        tidyr$pivot_longer(dplyr$everything()) |>
        dplyr$mutate(
          name  = stringr$str_to_upper(stringr$str_remove(name, "pos_"))
        )
      
      bslib$layout_columns(
        col_widths = c(6, 6),
        shiny$tagList(
          shiny$h4(paste("TPE:", data$tpe)),
          shiny$h4(paste("Banked TPE:", data$tpebank)),
          shiny$h4("Player Status:", data$playerStatus, class = data$playerStatus),
          shiny$h4("User Status:",   data$userStatus,   class = data$userStatus),
          shiny$h5("Nationality:",    data$nationality),
          shiny$h5("Render:",         data$render),
          shiny$h5(paste("Footedness "),
                   shiny$div(
                     style = "display: flex; gap: 5px; align-items: center; margin-top: 5px;",
                     
                     shiny$tags$svg(
                       xmlns = "http://www.w3.org/2000/svg",
                       shiny$tags$title(paste0("Left foot: ", data$`left foot`)),
                       width = "40", height = "40", viewBox = "0 0 100 100",
                       shiny$tags$path(
                         d = "M 65.793945 6.763916 A 8.7670002 8.7670002 0 0 0 57.0271 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 24.298096 A 8.7670002 8.7670002 0 0 0 74.561035 15.531006 A 8.7670002 8.7670002 0 0 0 65.793945 6.763916 z M 47.711914 12.860107 A 5.4689999 5.4689999 0 0 0 42.24292 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 23.798096 A 5.4689999 5.4689999 0 0 0 53.180908 18.329102 A 5.4689999 5.4689999 0 0 0 47.711914 12.860107 z M 36.160889 22.351074 A 4.342 4.342 0 0 0 31.819092 26.693115 A 4.342 4.342 0 0 0 36.160889 31.034912 A 4.342 4.342 0 0 0 40.50293 26.693115 A 4.342 4.342 0 0 0 36.160889 22.351074 z M 55.288086 27.615967 C 43.962086 27.615967 34.781006 36.797047 34.781006 48.123047 C 34.781006 50.335047 35.141039 52.461008 35.790039 54.458008 C 36.226039 56.250008 36.92891 57.932994 37.85791 59.468994 L 37.761963 59.523926 L 52.615967 85.25293 C 54.183967 89.89293 58.562957 93.237061 63.730957 93.237061 C 70.214957 93.237061 75.474121 87.981094 75.474121 81.496094 C 75.474121 78.683094 74.482055 76.102078 72.831055 74.080078 L 72.903076 74.042969 C 71.516076 71.805969 70.700928 69.176098 70.700928 66.350098 C 70.700928 63.886098 71.317066 61.568029 72.393066 59.530029 L 72.358887 59.479004 C 74.526887 56.227004 75.794922 52.324047 75.794922 48.123047 C 75.794922 36.797047 66.613086 27.615967 55.288086 27.615967 z M 28.549072 31.542969 A 4.342 4.342 0 0 0 24.207031 35.88501 A 4.342 4.342 0 0 0 28.549072 40.227051 A 4.342 4.342 0 0 0 32.891113 35.88501 A 4.342 4.342 0 0 0 28.549072 31.542969 z ",
                         fill = dplyr$case_when(
                           data$`left foot` == 20 ~ constant$green,
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
                           data$`right foot` == 20 ~ constant$green,
                           data$`right foot` == 15 ~ constant$yellow,
                           TRUE ~ constant$red
                         )
                       )
                     )
                   )
          )
        ),
        shiny$tagList(
          shiny$h4("Traits"),
          shiny$HTML(paste(stringr$str_split(data$traits, constant$traitSep)[[1]], collapse = "<br>")),
          shiny$br(), 
          shiny$h4("Primary Position(s)"),
          shiny$HTML(paste(
            posTbl |> dplyr$filter(value==20) |> dplyr$pull(name),
            collapse=", "
          )),
          shiny$h4("Secondary Position(s)"),
          shiny$HTML(paste(
            posTbl |> dplyr$filter(value<20, value>=10) |> dplyr$pull(name),
            collapse=", "
          ))
        )
      )
    }) |> 
      shiny$bindCache(updated())
    
    ## Cost placeholder (will be updated by store module)
    output$cost <- shiny$renderText("0")
  })
}
