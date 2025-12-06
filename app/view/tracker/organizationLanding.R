box::use(
  bslib,
  dplyr,
  shiny,
  shiny.router[route_link],
)

box::use(
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
        shiny$h1("Organization Overview")
      ),
      bslib$card_body(
        shiny$uiOutput(ns("orgButtons"))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Loading data
    organizations <- constant$organizations |>
      dplyr$filter(!is.na(organization), ID > -3)

    ### Outputs
    output$orgButtons <- shiny$renderUI({
      orgs <- organizations |> 
        dplyr$group_by(ID) |> 
        dplyr$summarize(
          name = paste(name, collapse = " / ")
        ) |> 
        dplyr$ungroup()
      
      shiny$div(
        class = "org-list",
        shiny$tagList(
          lapply(seq_len(nrow(orgs)), function(i) {
            oid <- orgs$ID[i]
            org <- orgs$name[i]
            
            maj <- 
              organizations |> 
              dplyr$filter(ID == oid) |> 
              dplyr$slice_head(n = 1) |> 
              dplyr$pull(name)
            
            min <- 
              organizations |> 
              dplyr$filter(ID == oid) |> 
              dplyr$slice_tail(n = 1) |> 
              dplyr$pull(name)
            
            
            shiny$tags$a(
              href = route_link(paste0("organization?oid=", oid)),
              target = "_self",   # open in same tab; use "_blank" for new tab
              shiny$actionButton(
                inputId = session$ns(paste0("org_", oid)),
                label   = 
                  shiny$div(
                    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                    
                    # left logo
                    shiny$span(
                      style = "flex: 0 0 auto;",
                      shiny$img(
                        src   = sprintf("static/logo/%s (Custom).png", maj),
                        style = "
                          filter: drop-shadow(2px 2px 4px rgba(0,0,0,0.8)); 
                          height: 50px;",
                        alt   = maj,
                        title = maj
                      )
                    ),
                    
                    # center text
                    shiny$span(
                      style = "flex: 1; text-align: center;",
                      org
                    ),
                    
                    # right logo (only if different)
                    if (min != maj) {
                      shiny$span(
                        style = "flex: 0 0 auto;",
                        shiny$img(
                          src   = sprintf("static/logo/%s (Custom).png", min),
                          style = "
                          filter: drop-shadow(2px 2px 4px rgba(0,0,0,0.8));
                          height: 50px;",
                          alt   = min,
                          title = min
                        )
                      )
                    }
                  ),
                style   = 
                  paste0(
                    "background-color: ", 
                    organizations |> 
                      dplyr$filter(ID == oid) |> 
                      dplyr$slice_head(n = 1) |> 
                      dplyr$pull(primaryColor), 
                    "; color: white; margin: 5px;"
                  )
              )
            )
          })
        )
      )
    })
  })
}
