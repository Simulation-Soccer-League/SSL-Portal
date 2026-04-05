box::use(
  bslib,
  dplyr,
  glue,
  purrr[pmap_chr,],
  reactable[
    colDef,
    colFormat,
    JS,
    reactable,
    reactableOutput, 
    renderReactable
  ],
  scales[dollar],
  shiny,
  shinyjs,
  shiny.router[get_query_param],
  stringi[stri_remove_empty],
  tidyr[pivot_wider,]
)

box::use(
  app/logic/constant[currentSeason,],
  app/logic/db/get[
    getOrganizationPlayers, 
    getOrgBudget,
    getTeamInformation,
  ],
  app/logic/ui/reactableHelper[
    orgReactable
  ],
  app/logic/ui/spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        shiny$h2("Organization")
      ),
      bslib$card_body(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 1fr 1fr 1fr"),
          shiny$uiOutput(ns("clubLogo"), height = NULL) |>
            withSpinnerCustom(height = 200),
          shiny$tagList(
            shiny$uiOutput(ns("city")) |> 
              withSpinnerCustom(height = 50),
            shiny$uiOutput(ns("orgInfo")) |>
              withSpinnerCustom(height = 50)
          ),
          shiny$tagList(
            shiny$uiOutput(ns("stadium")) |> 
              withSpinnerCustom(height = 50),
            shiny$uiOutput(ns("colors")) |> 
              withSpinnerCustom(height = 50)
          ),
          shiny$tagList(
            shiny$uiOutput(ns("established")) |> 
              withSpinnerCustom(height = 50)
          )
        )
      )
    ),
    shiny$uiOutput(ns("tabs")) |> 
      withSpinnerCustom(height = 50),
    shiny$br()
  )
}

#' @export
server <- function(id, oid = NULL, updated) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    query <- shiny$reactive({
      if (oid |> is.null()) {
        oid <- get_query_param("oid")
        
        if (is.null(oid)) {
          NULL
        } else {
          oid |>
            as.numeric()
        }
      } else {
        oid
      }
    })
    
    players <- shiny$reactive({
      shiny$req(query())
      
      getOrganizationPlayers(oid = query()) |>
        dplyr$select(
          name,
          class,
          position,
          tpe,
          tpebank,
          username,
          discord,
          bankBalance,
          nationality,
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
    }) |> 
      shiny$bindCache(id, query(), updated()) |> 
      shiny$bindEvent(query())
    
    teamInfo <- shiny$reactive({
      shiny$req(query())
      
      getTeamInformation(oid = query()) |> 
        dplyr$arrange(affiliate)
    }) |> 
      shiny$bindCache(id, query(), updated()) |> 
      shiny$bindEvent(query())
    
    budget <- shiny$reactive({
      shiny$req(query())
      
      getOrgBudget(oid = query()) |> 
        dplyr$mutate(
          flags = pmap_chr(
            list(vet, maj, nmc, rcc),
            ~ {
              f <- c(
                if (..1 == 1) "VET",
                if (..2 == 1) "MAJ",
                if (..3 == 1) "NMC",
                if (..4 == 1) "RCC"
              )
              if (length(f) == 0) "" else paste0(" (", paste(f, collapse = ", "), ")")
            }
          ),
          salaryText = 
            sprintf("%s%s",
                    salary |> dollar(),
                    flags
            )
          ) |> 
        pivot_wider(names_from = season, 
                    names_glue = "{season}_{.value}",
                    values_from = c(salary, salaryText)
                  )
    }) |> 
      shiny$bindCache(id, query(), updated()) |> 
      shiny$bindEvent(query())
    
    majors <- shiny$reactive({
      players() |>
        dplyr$filter(affiliate == 1)
    })
    
    minors <- shiny$reactive({
      players() |>
        dplyr$filter(affiliate == 2)
    })

    #### Output ####
    output$stadium <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo()
      
      stadiums <- c(
        dplyr$if_else(
          data$stadium[1] |> is.na(),
          "",
          data$stadium[1]
        ),
        dplyr$if_else(
          data$stadium[2] |> is.na(),
          "",
          data$stadium[2]
        )
      ) |> 
        stri_remove_empty()
      
      if (stadiums |> length() > 0) {
        # Build a nice list
        shiny$div(
          style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
          shiny$h4("Stadium", style = "margin-bottom: 6px;"),
          lapply(stadiums, function(name) {
            shiny$div(
              style = "padding: 4px 8px;
                width: fit-content;",
              name
            )
          })
        )
      }
    })
    
    output$established <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo()
      
      established <- c(
        dplyr$if_else(
          data$established[1] |> is.na(),
          "",
          paste0("S", data$established[1])
        ),
        dplyr$if_else(
          data$established[2] |> is.na(),
          "",
          paste0("S", data$established[2])
        )
      ) |> 
        stri_remove_empty()
      
      if (established |> length() > 0) {
        # Build a nice list
        shiny$div(
          style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
          shiny$h4("Established", style = "margin-bottom: 6px;"),
          lapply(established, function(name) {
            shiny$div(
              style = "padding: 4px 8px;
                width: fit-content;",
              name
            )
          })
        )
      }
    })
    
    output$tabs <- shiny$renderUI({
      majorName <- teamInfo() |> 
        dplyr$filter(affiliate == 1) |> 
        dplyr$pull(name)
      minorName <- teamInfo() |> 
        dplyr$filter(affiliate == 2) |> 
        dplyr$pull(name)
      
      shiny$tabsetPanel(
        shiny$tabPanel(
          title = paste(majorName, dplyr$if_else(query() < 0, "", "(Major)")),
          reactableOutput(session$ns("major"), height = (nrow(majors()) + 1.40) * 35) |> 
            withSpinnerCustom(height = 50)
        ),
        if (nrow(minors()) > 0) {
          shiny$tabPanel(
            title = paste(minorName, "(Minor)"),
            reactableOutput(session$ns("minor"), height = (nrow(minors()) + 1.40) * 35) |> 
              withSpinnerCustom(height = 50)
          )
        },
        if (query() >= 0) {
          shiny$tabPanel(
            title = "Budget",
            reactableOutput(session$ns("budget"), height = (nrow(budget()) + 3) * 35) |> 
              withSpinnerCustom(height = 50)
          )
        }
      )
    })
    
    output$major <- renderReactable({
      orgReactable(majors())
    })
    
    output$minor <- renderReactable({
      orgReactable(minors())
    })
    
    
    output$budget <- renderReactable({
      seasons <- currentSeason$season:(currentSeason$season + 4)
      
      seasonTextCols <- lapply(seasons, function(season) {
        # Create a colDef for the visible text column
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
        # Create a colDef for the visible text column
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
      
      budget() |> 
        reactable(
          groupBy = "team",
          defaultExpanded = TRUE,
          defaultColDef = colDef(show = FALSE),
          pagination = FALSE,
          columns = c(
            list(
              team = colDef(name = "Roster", show = TRUE),
              name = colDef(name = "Player", show = TRUE),
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
    })
    
    output$clubLogo <- shiny$renderUI({
      data <- teamInfo() |> 
        dplyr$arrange(affiliate)
      
      shiny$tagList(
        shiny$div(
          style = glue$glue(
            "display: flex; 
             flex-direction: column; 
             align-items: center; /* center horizontally */ 
             justify-content: space-between; 
             width: 150px; 
             height: {height}px; 
             margin: 0 auto;",
            height = nrow(data) * 150
          ),
          # Top-left image
          if (nrow(data) >= 1) {
            shiny$div(
              shiny$img(
                src = sprintf("static/logo/%s.png", data$name[1]),
                style = "height: 150px; padding: 2px;",
                alt = data$name[1],
                title = data$fullname[1]
              )
            )
          },
          # Bottom-right image
          if (nrow(data) >= 2) {
            shiny$div(
              shiny$img(
                src = sprintf("static/logo/%s.png", data$name[2]),
                style = "height: 150px; padding: 2px;",
                alt = data$name[2],
                title = data$fullname[2]
              )
            )
          }
        )
      )
        
      
    })
    
    output$orgInfo <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo()
      
      # Extract up to three names (already joined from allplayersview)
      managers <- c(
        paste(
          "Organizational Manager:",
          dplyr$if_else(data$om[1] |> is.na(), "None", data$om[1])
        ),
        paste(
          "Assistant Manager:",
          dplyr$if_else(data$am1[1] |> is.na(), "None", data$am1[1])
        ),
        paste(
          "Assistant Manager:",
          dplyr$if_else(data$am2[1] |> is.na(), "None", data$am2[1])
        )
      )
      
      # Build a nice list
      shiny$div(
        style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
        shiny$h4("Management", style = "margin-bottom: 6px;"),
        lapply(managers, function(name) {
          shiny$div(
            style = "padding: 4px 8px;
                width: fit-content;",
            name
          )
        })
      )
    })
    
    output$city <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo()
      
      cities <- c(
        dplyr$if_else(
          data$city[1] |> is.na(),
          "",
          data$city[1]
        ),
        dplyr$if_else(
          data$city[2] |> is.na(),
          "",
          data$city[2]
        )
      ) |> 
        stri_remove_empty()
        
      if (cities |> length() > 0) {
        # Build a nice list
        shiny$div(
          style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
          shiny$h4("Origin", style = "margin-bottom: 6px;"),
          lapply(cities, function(name) {
            shiny$div(
              style = "padding: 4px 8px;
                width: fit-content;",
              name
            )
          })
        )
      }
      
    })
    
    output$colors <- shiny$renderUI({
      shiny$req(teamInfo())
      
      data <- teamInfo() |> 
        dplyr$select(primaryColor, secondaryColor)
      
      # Build a nice list
      shiny$div(
        style = "
          padding: 10px 0;
          display: flex;
          flex-direction: column;
          gap: 4px;
        ",
        shiny$h4("Colors", style = "margin-bottom: 6px;"),
        lapply(seq_len(nrow(data)), function(index) {
          shiny$div(
            style = 
              "display: flex; 
                flex-direction: row; 
                gap: 8px;",
            shiny$div(
              style = glue$glue(
                "padding: 4px 8px;
                  background: {bg};
                  color: {col};
                  width: fit-content;",
                bg = data$primaryColor[index],
                col = data$secondaryColor[index]
              ),
              data$primaryColor[index]
            ),
            shiny$div(
              style = glue$glue(
                "padding: 4px 8px;
                  background: {bg};
                  color: {col};
                  width: fit-content;",
                bg = data$secondaryColor[index],
                col = data$primaryColor[index]
              ),
              data$secondaryColor[index]
            )
          )
        })
      )
      
    })
  })
}
