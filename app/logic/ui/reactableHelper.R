box::use(
  dplyr,
  purrr[map, pmap],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  shiny.router[route_link],
  shiny[a, div, h4, img, span, tagList],
  stats[setNames],
  stringr[str_detect, str_split, str_to_title, str_to_upper, str_trim],
  tidyr[pivot_longer],
  tippy[tippy],
)

box::use(
  app / logic / constant,
)

#' @export
clubLogos <- function(value, index, currentData) {
  clubData <- currentData |>
    dplyr$select(club) |>
    dplyr$slice(index)

  clubData <- clubData$club

  if (clubData |> str_detect(",")) {
    clubs <- str_split(clubData, pattern = ",", simplify = TRUE) |>
      c() |>
      str_trim() |>
      rev()

    list <-
      tagList(
        lapply(
          clubs,
          function(x) {
            div(
              style = "display: inline-block; width: 25px;",
              img(
                src = sprintf("static/logo/%s (Custom).png", x),
                style = "height: 25px;",
                alt = x,
                title = x
              )
            )
          }
        )
    }
    
    tagList(
      div(
        class = "table-club-name",
        span(value),
        div(list)
      )
  } else {
    image <- img(
      src = sprintf("static/logo/%s (Custom).png", clubData),
      style = "height: 25px;",
      alt = clubData,
      title = clubData
    )

    list <-
      tagList(
        div(style = "display: inline-block; width: 25px;", image)
      )
  }

  tagList(
    div(
      class = "tableClubName",
      span(value),
      div(list)
    )
  )
}

#' @export
draftClassReactable <- function(data) {
  data |>
    dplyr$rename_with(str_to_upper) |>
    dplyr$relocate(TEAM, POSITION) |>
    reactable(
      pagination = FALSE,
      columns = list(
        POSITION = colDef(name = "POS", width = 50),
        USERSTATUS = colDef(width = 150),
        PLAYERSTATUS = colDef(width = 150),
        TPE = colDef(width = 50),
        NAME = colDef(width = 150, cell = function(value, rowIndex) {
          pid <- data[rowIndex, "pid"] # Get the corresponding pid
          tippy(
            a(
              href = route_link(paste0("tracker/player?pid=", pid)),
              value # Display the name as the link text
            ),
            tooltip = value,
            theme = "ssl"
          )
        }),
        USERNAME = colDef(
          cell = function(value) {
            tippy(
              value,
              tooltip = value,
              theme = "ssl"
            )
          },
        ),
        BANKBALANCE = colDef(
          width = 120, 
          format = colFormat(
            digits = 0,
            separators = TRUE,
            currency = "USD"
          )
        ),
        TEAM = colDef(
          name = "",
          width = 200,
          align = "left",
          cell = function(value) {
            image <- img(
              src = sprintf("static/logo/%s (Custom).png", value),
              style = "height: 30px;",
              alt = value,
              title = value
            )
            list <-
              tagList(
                div(
                  class = "table-club-name",
                  div(style = "display: inline-block; width: 30px;", image),
                  span(value)
                )
              )
          }
        ),
        PID = colDef(show = FALSE)
      )
    )
}

#' @export
recordReactable <- function(currentData) {
  statisticsTooltips <-
    constant$statisticsLegend[constant$statisticsLegend$statistic %in% colnames(currentData), ]

  currentData |>
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.numeric),
        ~ round(.x, 2)
      )
    ) |>
    reactable(
      pagination = FALSE,
      searchable = FALSE,
      sortable = FALSE,
      defaultColDef = colDef(maxWidth = 150),
      columns =
        list(
          name = colDef(
            name = "PLAYER",
            maxWidth = 1000,
            cell = function(value, index) {
              clubLogos(value, index, currentData)
            }
          ),
          club = colDef(show = FALSE, searchable = TRUE),
          RANK = colDef(width = 60),
          matchday =
          colDef(
            header = "MATCHDAY",
            width = 100
          )
        ) |>
        append(
          pmap(statisticsTooltips, ~ {
            if ((..1) %in% names(currentData)) {
              ..1 <- # nolint: object_name_linter
                colDef(
                  header =
                  tippy(..3 |> str_to_upper(), ..2, placement = "top", theme = "ssl"),
                  html = TRUE,
                  minWidth = 50
                )
            }
          }) |>
            setNames(statisticsTooltips$statistic) |>
            Filter(f = Negate(is.null), x = _)
        )
    )
}

#' @export
indexReactable <- function(currentData) {
  statisticsTooltips <-
    constant$statisticsLegend[constant$statisticsLegend$statistic %in% colnames(currentData), ]

  currentData |>
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.numeric),
        ~ round(.x, 2)
      )
    ) |>
    reactable(
      pagination = TRUE,
      searchable = TRUE,
      defaultColDef = colDef(minWidth = 100, maxWidth = 250, searchable = FALSE),
      columns =
        list(
          name = colDef(
            name = "PLAYER",
            minWidth = 250,
            class = "sticky-reactable-column",
            headerClass = "sticky-reactable-header",
            searchable = TRUE,
            cell = function(value, index) {
              clubLogos(value, index, currentData)
            }
          ),
          club =
          colDef(
            show = FALSE,
            searchable = TRUE
          )
        ) |>
        append(
          pmap(statisticsTooltips, ~ {
            if ((..1) %in% names(currentData)) {
              ..1 <- # nolint: object_name_linter
                colDef(
                  header =
                  tippy(..1 |> str_to_upper(), ..2, placement = "top", theme = "ssl"),
                  html = TRUE
                )
            }
          }) |>
            setNames(statisticsTooltips$statistic) |>
            Filter(f = Negate(is.null), x = _)
        )
    )
}

#' @export
orgReactable <- function(data) {
  reactable(
    data,
    defaultColDef = colDef(header = function(value) {
      str_to_upper(value)
    }),
    pagination = FALSE,
    columns = list(
      bankBalance = colDef(
        width = 120,
        format = colFormat(
          digits = 0,
          separators = TRUE,
          currency = "USD"
        )
      ),
      team = colDef(show = FALSE),
      affiliate = colDef(show = FALSE),
      name = colDef(width = 150, cell = function(value, rowIndex) {
        pid <- data[rowIndex, "pid"] # Get the corresponding pid
        tippy(
          a(
            href = route_link(paste0("tracker/player?pid=", pid)),
            value # Display the name as the link text
          ),
          tooltip = value,
          theme = "ssl"
        )
      }),
      username = colDef(
        width = 120, 
        cell = function(value) tippy(value, tooltip = value, theme = "ssl")
      ),
      discord = colDef(
        width = 120, 
        cell = function(value) tippy(value, tooltip = value, theme = "ssl")
      ),
      render = colDef(
        width = 150, 
        cell = function(value) tippy(value, tooltip = value, theme = "ssl")
      ),
      class = colDef(width = 75),
      tpe = colDef(width = 50),
      tpebank = colDef(width = 75),
      userStatus = colDef(width = 125),
      playerStatus = colDef(width = 140),
      pid = colDef(show = FALSE)
    )
  )
}

#' @export
attributeReactable <- function(data, session, output) {
  processedData <-
    data |>
    dplyr$select(acceleration:throwing) |>
    dplyr$select(
      dplyr$where(~ !is.na(.x))
    ) |>
    pivot_longer(
      cols = dplyr$everything(),
      values_to = "Value",
      names_to = "Attribute"
    ) |>
    dplyr$mutate(
      Attribute = str_to_title(Attribute)
    ) |>
    dplyr$left_join(
      constant$attributes,
      by = c("Attribute" = "attribute")
    ) |>
    dplyr$mutate(
      Attribute = factor(Attribute, levels = sort(Attribute |> unique(), decreasing = TRUE)),
      group = factor(group, levels = c("Physical", "Mental", "Technical", "Goalkeeper")),
      ValueFill = dplyr$case_when(
        Value >= 18 ~ 1,
        Value >= 13 ~ 2,
        Value >= 10 ~ 3,
        TRUE ~ 5
      ) |> factor()
    ) |>
    dplyr$filter(
      if (data$pos_gk == 20) {
        (
          (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE")
          | (group %in% c("Physical", "Mental"))
        )
      } else {
        group %in% c("Physical", "Mental", "Technical")
      }
    )

  map(
    .x = processedData$group |> unique() |> sort(),
    .f = function(chosenGroup) {
      output[[chosenGroup]] <- renderReactable({
        temp <-
          processedData |>
          dplyr$filter(
            group == chosenGroup
          )

        temp |>
          dplyr$select(Attribute, Value) |>
          reactable(
            defaultColDef = colDef(
              style = function(value, index) {
                color <- dplyr$if_else(temp$ValueFill[index] == 1, constant$green,
                  dplyr$if_else(temp$ValueFill[index] == 2, constant$yellow,
                    dplyr$if_else(temp$ValueFill[index] == 3, "#ffffff", "#B6B6B6")
                  )
                )
                list(background = color, color = "black")
              }
            ),
            columns = list(
              Value = colDef(name = "", width = 40)
            ),
            pagination = FALSE,
            sortable = FALSE
          )
      })
    }
  )

  map(
    .x = processedData$group |> unique() |> sort(),
    .f = function(chosenGroup) {
      tagList(
        div(
          style = "width: 80%",
          h4(chosenGroup),
          reactableOutput(session$ns(chosenGroup))
        )
      )
    }
  ) |> 
    div(class = "attribute-tables")
}
