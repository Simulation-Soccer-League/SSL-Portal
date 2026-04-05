box::use(
  dplyr,
  glue,
  purrr[is_empty, map, pmap],
  reactable[
    colDef, 
    colFormat, 
    JS, 
    reactable, 
    reactableOutput, 
    renderReactable,
  ],
  shiny.router[route_link],
  shiny[a, div, h4, img, span, tagList],
  stats[setNames],
  stringr[str_detect, str_split, str_to_title, str_to_upper, str_trim],
  tidyr[pivot_longer],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/ui/tags[flexRow],
)

#' @export
clubLogos <- function(value, index, currentData, onlyLogo = FALSE) {
  clubData <- currentData |>
    dplyr$select(club, pid) |>
    dplyr$slice(index)
  
  pid <- clubData$pid
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
      )
    
  } else {
    list <- 
      linkOrganization(clubData, onlyImg = TRUE)
  }
  
  tagList(
    div(
      class = "tableClubName",
      if (!onlyLogo & !is.na(pid)) {
        tagList(
          a(
            href = route_link(paste0("tracker/player?pid=", pid)),
            value # Display the name as the link text
          ),
          div(list)
        )
      } else if (!onlyLogo) {
        tagList(
          value,
          div(list)
        )
      } else {
        div(list)
      }
    )
  )
}

#' @export
linkOrganization <- function(value, onlyImg = FALSE, height = 30) {
  oid <- constant$organizations |> 
    dplyr$filter(name == value) |> 
    dplyr$pull(ID) |> 
    unique()
  
  # Grabs the higher res image if larger than 80
  if (height > 80) {
    version <- ""
  } else {
    version <- " (Custom)"
  }
  
  image <- 
    img(
      src = sprintf("static/logo/%s%s.png", value, version),
      style = paste0("height: ", height, "px;"),
      alt = value,
      title = value
    )
  
  if (onlyImg) {
    list <- 
      tagList(
        image
      )
  } else {
    list <- 
      flexRow(
        style = "align-items: center; gap: 8px;",
        tagList(
          image,
          span(class = "truncated-text", value)
        )
      )
  }
  
  if ((oid |> is_empty()) | (oid |> is.na() |> isTRUE())){
    if (onlyImg) {
      list
    } else {
      tagList(
        div(
          class = "tableClubName",
          list
        )
      )
    }
  } else if (onlyImg) {
    list <-
      tagList(
        a(
          href = route_link(paste0("organization?oid=", oid)),
          list
        )
      )
  } else {
    list <-
      tagList(
        a(
          href = route_link(paste0("organization?oid=", oid)),
          class = "tableClubName",
          list
        )
      )
  }
  
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
            oid <- constant$organizations |> 
              dplyr$filter(name == value) |> 
              dplyr$pull(ID) |> 
              unique()
            
            image <- img(
              src = sprintf("static/logo/%s (Custom).png", value),
              style = "height: 30px;",
              alt = value,
              title = value
            )
            list <-
              tagList(
                a(
                  href = route_link(paste0("organization?oid=", oid)),
                  class = "tableClubName",
                  flexRow(
                    style = "align-items: center; gap: 8px;",
                    tagList(
                      image,
                      span(class = "truncated-text", value)
                    )
                  )
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
          result = colDef(
            header =
              tippy(
                statisticsTooltips$abbreviation[statisticsTooltips$statistic == "result"] |> str_to_upper(), 
                statisticsTooltips$explanation[statisticsTooltips$statistic == "result"], 
                placement = "top", 
                theme = "ssl"
                ),
            html = TRUE,
            minWidth = 50,
            cell = function(value, index) {
              a(
                href = route_link(
                  sprintf(
                    "tracker/game?gid=%s", 
                    currentData$gid[index]
                    )
                  ),
                value
              )
            }
          ),
          club = colDef(show = FALSE, searchable = TRUE),
          RANK = colDef(width = 60),
          matchday = colDef(header = "MATCHDAY", width = 100),
          pid = colDef(show = FALSE),
          gid = colDef(show = FALSE)
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
indexReactable <- function(currentData, search = TRUE, club = FALSE, ...) {
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
      ...,
      searchable = search,
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
              name = "CLUB",
              width = 60,
              show = club,
              searchable = TRUE,
              cell = function(value, index) {
                clubLogos(value, index, currentData, onlyLogo = TRUE)
              }
            ),
          pid = colDef(show = FALSE)
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
      tippy(
        str_to_upper(value), 
        tooltip = str_to_title(value), 
        theme = "ssl"
      )
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
      name = colDef(
        width = 175, 
        sticky = "left",
        cell = function(value, rowIndex) {
          pid <- data[rowIndex, "pid"] # Get the corresponding pid
          tippy(
            a(
              href = route_link(paste0("tracker/player?pid=", pid)),
              value # Display the name as the link text
            ),
            tooltip = value,
            theme = "ssl"
          )
        }
      ),
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
      userStatus = colDef(width = 125),
      playerStatus = colDef(width = 140),
      pid = colDef(show = FALSE)
    )
  )
}

#' @export
budgetReactable <- function(budget) {
  
  seasons <- constant$currentSeason$season:(constant$currentSeason$season + 4)
  
  seasonTextCols <- lapply(seasons, function(season) {
    # Create a colDef for the visible text columns
    colDef(
      name = sprintf("S%s", as.character(season)),
      show = TRUE,
      aggregate = JS(
        sprintf(
          "function(values, rows) {
                let totalSalary = 0;
                
                rows.forEach(function(row)  {
                  totalSalary += row['%s_salary'];
                });
                
                return new Intl.NumberFormat('en-US', {
                  style: 'currency',
                  currency: 'USD',
                  minimumFractionDigits: 0
                }).format(totalSalary);
              }",
          season
        ) 
      ),
      style = JS(
        sprintf(
          "function(rowInfo) {
                var value = rowInfo.row['%s_salary']
                var affiliate = rowInfo.row['affiliate']
                
                if ( (affiliate == 1 && value > 55E6) || 
                      (affiliate == 2 & value > 45E6) ){
                  color = '#a9322678'
                } else {
                  color = '#ffffff00'
                }
                
                return { background: color }
              }",
          season
        ) 
      )
    )
  })
  
  names(seasonTextCols) <- sprintf("%s_salaryText", seasons)
  
  seasonCols <- lapply(seasons, function(season) {
    # Create a colDef for the invisible numeric columns
    colDef(
      name = sprintf("%s_salary", as.character(season)),
      show = FALSE,
      aggregate = JS(
        sprintf(
          "function(values, rows) {
                let totalSalary = 0;
                
                rows.forEach(function(row)  {
                  totalSalary += row['%s_salary'];
                });
                
                return totalSalary;
              }",
          season
        ) 
      )
    )
  })
  
  names(seasonCols) <- sprintf("%s_salary", seasons)
  
  budget |> 
    reactable(
      groupBy = "team",
      defaultExpanded = TRUE,
      defaultColDef = colDef(show = FALSE),
      pagination = FALSE,
      columns = c(
        list(
          team = colDef(name = "Roster", show = TRUE),
          name = 
            colDef(
              name = "Player", 
              show = TRUE,
              cell = function(value, rowIndex) {
                pid <- budget[rowIndex, "pid"] # Get the corresponding pid
                tippy(
                  a(
                    href = route_link(paste0("tracker/player?pid=", pid)),
                    value # Display the name as the link text
                  ),
                  tooltip = value,
                  theme = "ssl"
                )
              }
            ),
          affiliate = colDef(aggregate = "mean"),
          ia = colDef(
            name = "IA Contract Status", 
            show = TRUE, 
            aggregate = JS(
              "function(values, rows) {
                    let totalIA = 0;
                    
                    rows.forEach(function(row)  {
                      totalIA += row['ia'];
                    });
                    
                    return totalIA;
                  }"
            ),
            style = JS(
              "function(rowInfo) {
                    var value = rowInfo.row['ia']
                    var affiliate = rowInfo.row['affiliate']
                    
                    if (affiliate == 1 && value > 3) {
                      color = '#a9322678'
                    } else {
                      color = '#ffffff00'
                    }
                    
                    return { background: color }
                  }"
            ),
            cell = function (value) {
              if (value == 0) "\u2714\ufe0f No" else "\u274c Yes"
            }
          )
        ), 
        seasonTextCols,
        seasonCols
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

#' @export
reactableBar <- function(value, total, side = "left", color = constant$sslBlueL){
  pct <- if (total == 0) 0 else (value / total) * 100 
  div(
    style = "
    background:var(--bottom-background);
    width: 100%;
    height:16px; 
    position:relative;", 
    div(
      style = glue$glue(
        "background:{color}; 
        width:{pct}%; 
        height:100%;
        float:{side};"
      )
    ),
    div(
      style = glue$glue(
        "
        position: absolute;
        top: 0;
        bottom: 0;
        display: flex;
        align-items: center;
        color: white;
        font-size: 12px;
        font-weight: 600;
        padding: 0 4px;
        {if (side == 'right') 'right: 4px; justify-content: flex-end;' else 'left: 4px; justify-content: flex-start;'}
        "
      ),
      value |> round(3)
    )
  )
}




