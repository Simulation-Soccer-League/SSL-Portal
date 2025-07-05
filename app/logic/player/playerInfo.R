box::use(
  bslib,
  dplyr,
  shiny,
  stringr,
  tidyr,
)

box::use(
  app/logic/constant,
  app/logic/db/get[getActivePlayer, getPlayer],
  app/logic/player/playerChecks[hasActivePlayer],
  app/logic/db/login[isNonActiveForumUser],
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
      pos_tbl <- data |>
        dplyr$select(dplyr$starts_with("pos_")) |>
        tidyr$pivot_longer(dplyr$everything()) |>
        dplyr$mutate(
          name  = stringr$str_to_upper(stringr$str_remove(name, "pos_"))
        )
      
      bslib$layout_columns(
        col_widths = c(6,6),
        shiny$tagList(
          shiny$h4(paste("TPE:", data$tpe)),
          shiny$h4(paste("Banked TPE:", data$tpebank)),
          shiny$h4("Player Status:", data$playerStatus, class = data$playerStatus),
          shiny$h4("User Status:",   data$userStatus,   class = data$userStatus),
          shiny$h5("Nationality:",    data$nationality),
          shiny$h5("Render:",         data$render)
        ),
        shiny$tagList(
          shiny$h4("Traits"),
          shiny$HTML(paste(stringr$str_split(data$traits, constant$traitSep)[[1]], collapse = "<br>")),
          shiny$br(), 
          shiny$h4("Primary Position(s)"),
          shiny$HTML(paste(
            pos_tbl |> dplyr$filter(value==20) |> dplyr$pull(name),
            collapse=", "
          )),
          shiny$h4("Secondary Position(s)"),
          shiny$HTML(paste(
            pos_tbl |> dplyr$filter(value<20, value>=10) |> dplyr$pull(name),
            collapse=", "
          ))
        )
      )
    }) |> shiny$bindCache(updated())
    
    ## Cost placeholder (will be updated by store module)
    output$cost <- shiny$renderText({ "0" })
  })
}
