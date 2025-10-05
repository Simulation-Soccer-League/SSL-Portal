box::use(
  bslib,
  dplyr,
  shiny,
)

box::use(
  app / logic / db / get[getOrganizations, getPlayers],
  app / logic / ui / reactableHelper[orgReactable],
  app / logic / ui / spinner[withSpinnerCustom],
  app / logic / ui / tags[flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$h1("Organization Overview")
      ),
      bslib$card_body(
        shiny$uiOutput(ns("tabs"))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Loading data
    players <- shiny$reactive({
      getPlayers(active = FALSE) |>
        dplyr$select(
          name,
          class,
          tpe,
          tpebank,
          username,
          discord,
          bankBalance,
          nationality,
          position,
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
    })

    organizations <- getOrganizations() |>
      dplyr$filter(!is.na(organization), ID > -3)

    ### Outputs
    output$tabs <- shiny$renderUI({
      do.call(
        shiny$tabsetPanel,
        c(
          list(width = NULL, selected = "CA Buenos Aires"),
          lapply(unique(organizations$organization), function(org) {
            shiny$tabPanel(
              title = org,
              flexRow(
                shiny$uiOutput(
                  session$ns(
                    paste0(
                      "overview_",
                      unique(organizations$ID[organizations$organization == org])
                    )
                  )
                ) |> 
                  withSpinnerCustom(height = 60)
              )
            )
          })
        )
      )
    })

    ### Observer
    shiny$observe({
      data <- players()

      lapply(unique(organizations$ID), function(i) {
        output[[paste0("overview_", i)]] <- shiny$renderUI({
          majors <- data |>
            dplyr$filter(team %in% organizations$name[organizations$ID == i] & affiliate == 1)
          minors <- data |>
            dplyr$filter(team %in% organizations$name[organizations$ID == i] & affiliate == 2)

          majorName <- majors$team |> unique()
          minorName <- minors$team |> unique()

          shiny$tagList(
            shiny$h3(
              paste(majorName, dplyr$if_else(i < 0, "", "(Major)"))
            ),
            flexRow(
              orgReactable(majors)
            ),
            if (nrow(minors) > 0) {
              shiny$tagList(
                shiny$h3(
                  paste(minorName, "(Minor)")
                ),
                flexRow(
                  orgReactable(minors)
                )
              )
            }
          )
        })
      })
    })
  })
}
