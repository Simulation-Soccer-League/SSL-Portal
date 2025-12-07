box::use(
  bslib,
  dplyr,
  highcharter[
    color_classes,
    hc_add_series_map,
    hc_colorAxis,
    hc_mapNavigation,
    hc_title,
    hc_tooltip,
    highchart,
    highchartOutput,
    renderHighchart,
    worldgeojson,
  ],
  shiny,
  tidyr[
    pivot_wider,
    replace_na,
  ],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getPlayers],
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
        shiny$h1("Nation Tracker of Players")
      ),
      bslib$card_body(
        highchartOutput(ns("map")) |>  
          withSpinnerCustom(height = 60),
        shiny$h3("Regional Rosters"),
        shiny$uiOutput(ns("tabs")) |> 
          withSpinnerCustom(height = 60)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Loading data
    players <- shiny$reactive({
      getPlayers(active = TRUE) |>
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
          team,
          affiliate,
          region,
          pid
        )
    })
    
    nations <- shiny$reactive({
      players() |> 
        dplyr$mutate(
          nationality = dplyr$case_when(
            nationality == "United States" ~ "United States of America",
            nationality == "Korea, South" ~ "South Korea",
            nationality == "Czechia" ~ "Czech Republic",
            nationality == "Serbia" ~ "Republic of Serbia",
            nationality %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "United Kingdom",
            TRUE ~ nationality
          )
        ) |> 
        dplyr$group_by(nationality, userStatus, region) |> 
        dplyr$summarize(actives = dplyr$n()) |> 
        dplyr$group_by(nationality) |> 
        dplyr$mutate(n = sum(actives)) |> 
        dplyr$group_by(region) |> 
        dplyr$mutate(roster = sum(actives)) |> 
        dplyr$ungroup() |> 
        pivot_wider(
          names_from = userStatus, 
          values_from = actives
        ) |> 
        dplyr$mutate(
          dplyr$across(
            c(Inactive, Active), 
            ~replace_na(.x, 0)
          )
        )
    })

    ### Outputs
    output$tabs <- shiny$renderUI({
      do.call(
        shiny$tabsetPanel,
        c(
          list(width = NULL),
          lapply(unique(players()$region) |> sort(), function(org) {
            shiny$tabPanel(
              title = org,
              flexRow(
                shiny$uiOutput(
                  session$ns(paste0("overview_",org))
                )
              )
            )
          })
        )
      )
    })
    
    output$map <- renderHighchart({
      highchart(
        hc_opts = list(
          backgroundColor = "#000",
          zooming = list(type = "xy", singleTouch = TRUE)
        ),
        type = "map"
      ) |> 
        hc_title("Nation Tracker of Players") |>
        hc_add_series_map(
          name = "Nr. Players",
          worldgeojson, 
          nations(), 
          value = "n", 
          joinBy = c("name", "nationality")
        ) |>
        hc_colorAxis(
          dataClasses = 
            color_classes(
              c(1, 5, 10, 15, 20, 100),
              colors = c(constant$red, constant$yellow, constant$green)
            )
        ) |>
        hc_tooltip(HTML = TRUE, 
                   pointFormat = "{point.nationality} has {point.Active} active and {point.Inactive} inactive players.<br>In total the region {point.region} has {point.roster} players.") |> 
        hc_mapNavigation(enabled = TRUE)
      
    })

    ### Observer
    shiny$observe({
      data <- players()

      lapply(unique(data$region) |> sort(), function(i) {
        output[[paste0("overview_", i)]] <- shiny$renderUI({
          roster <- data |> 
            dplyr$filter(region == i)
          
          orgReactable(roster)
        })
      })
    })
  })
}
