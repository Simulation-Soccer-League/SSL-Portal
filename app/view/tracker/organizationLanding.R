box::use(
  bslib,
  dplyr,
  shiny,
  shiny.router[route_link],
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
        shiny$uiOutput(ns("orgButtons"))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Loading data
    organizations <- getOrganizations() |>
      dplyr$filter(!is.na(organization), ID > -3)

    ### Outputs
    output$orgButtons <- shiny$renderUI({
      orgs <- organizations |> 
        dplyr$group_by(ID) |> 
        dplyr$summarize(
          name = paste(name, collapse = " / ")
        ) |> 
        dplyr$ungroup()
      
      shiny$tagList(
        lapply(seq_len(nrow(orgs)), function(i) {
          oid <- orgs$ID[i]
          org <- orgs$name[i]
          
          shiny$tags$a(
            href = route_link(paste0("organization?oid=", oid)),
            target = "_self",   # open in same tab; use "_blank" for new tab
            shiny$actionButton(
              inputId = session$ns(paste0("org_", oid)),
              label   = org,
              style   = "margin: 5px;"
            )
          )
        })
      )
    })
  })
}
