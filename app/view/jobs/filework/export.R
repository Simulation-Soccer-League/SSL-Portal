box::use(
  bslib,
  dplyr[case_when, filter, if_else, mutate, select],
  glue[glue],
  lubridate[today],
  purrr[imap],
  reactable[reactable, reactableOutput, renderReactable],
  shiny,
  zip[zip],
)

box::use(
  app/logic/db/get[getChangedBuilds, getPlayer, getPlayerNames],
  app/logic/db/login[isBoD, isFileworker],
  app/logic/export[downloadPlayer],
  app/view/tracker/player,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server <- function(id, auth, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      names <- getPlayerNames(active = TRUE)

      namedVector <- names$pid
      names(namedVector) <- names$name

      shiny$tagList(
        bslib$layout_column_wrap(
          width = 1/3,
          shiny$p("This tool combines all updated builds from the last week into a zip-file. Each player is summarized in their own
             JSON file which can be imported using FMRTE. NOTE THAT NATIONALITIES ARE NOT INCLUDED IN THE JSON."),
          shiny$div(
            id = ns("playerDownload"),
            shiny$downloadButton(ns("downloadData"), "Download Updated Players")
          ),
          ""
        ),
        reactableOutput(ns("changes")),
        shiny$br(),
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "3fr 2fr"),
          shiny$div(
            shiny$h4("Select a player and download a single build:"),
            shiny$selectizeInput(
              inputId = ns("selectedPlayer"),
              label = "Select a player to export",
              choices = namedVector
            )
          ),
          shiny$div(
            id = ns("singlePlayerDownload"),
            shiny$downloadButton(ns("singleDownloadData"), "Download Selected Player")
          )
        )
      )
    })
    

    #### OUTPUT SERVER ####

    ## REACTIVES
    builds <- shiny$reactive({
      getChangedBuilds()
    })

    build <- shiny$reactive({
      getPlayer(input$selectedPlayer)
    }) |>
      shiny$bindCache(input$selectedPlayer)


    ## OUTPUT
    output$changes <- renderReactable({
      data <- builds()

      data |>
        select(
          Name = name,
          Team = teamName,
          Attribute,
          Previous = old,
          New = new
        ) |>
        filter(
          !(Attribute %in% c("Player Status", "Team"))
        ) |> 
        reactable(
          groupBy = c("Team", "Name")
        )
    })

    output$downloadData <- shiny$downloadHandler(
      filename = function(name) {
        paste(today(), "Builds.zip", sep = "")
      },
      content = function(file) {
        data <- builds()

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)

        data <-
          data |>
          select(!c(Attribute, old, new)) |>
          unique() |>
          mutate(
            fileName = paste(teamName, name, sep = "_")
          )

        data$fileName |>
          imap(function(x, y) {
            if (!is.null(x)) {
              player <- data |> filter(fileName == x)
              
              league <- if_else(data$affiliate == 1, "Major", "Minor")
              
              file_name <- glue("{x}_Build.json")
              writeLines(
                downloadPlayer(player),
                file.path(league, temp_directory, file_name)
              )
            }
          })

        zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
      },
      contentType = "application/zip"
    )

    output$singleDownloadData <- shiny$downloadHandler(
      filename = function(name) {
        paste(build()$name, " Build.json", sep = "")
      },
      content = function(file) {
        data <- build()

        writeLines(
          downloadPlayer(data),
          file
        )
      },
      contentType = "json"
    )
  })
}
