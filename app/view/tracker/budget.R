box::use(
  bslib,
  dplyr,
  purrr[pmap_chr,],
  reactable[reactableOutput, renderReactable],
  scales[dollar],
  shiny,
  tidyr[pivot_wider,],
)

box::use(
  app / logic / constant,
  app / logic / db / get[getOrgBudget, getOrganizations],
  app / logic / ui / reactableHelper[budgetReactable],
  app / logic / ui / spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          colwidths = c(2, 10)
        ),
        shiny$selectInput(
          inputId = ns("selectedOrg"),
          label = "Select an organization",
          choices = NULL
        ),
        ""
      ),
      bslib$card_body(
        shiny$h2("Budget"),
        shiny$uiOutput(ns("budgetUI")) |>
          withSpinnerCustom(height = 400)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Data
    organization <- shiny$reactive({
      orgs <- getOrganizations() |> 
        dplyr$filter(ID >= 0) |> 
        dplyr$select(ID, organization) |> 
        unique()
      
      id <- orgs$ID
      
      names(id) <- orgs$organization
      
      id
    })
    
    budget <- shiny$reactive({
      shiny$req(input$selectedOrg)
      
      getOrgBudget(oid = input$selectedOrg) |> 
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
      shiny$bindCache(id, input$selectedOrg) |> 
      shiny$bindEvent(input$selectedOrg)

    ### Observer
    
    shiny$observe({
      shiny$req(organization())
      
      shiny$updateSelectInput(
        inputId = "selectedOrg",
        choices = organization()
      )
    })
    
    ### Output
    output$budgetUI <- shiny$renderUI({
      reactableOutput(session$ns("budget"), height = (nrow(budget()) + 3) * 35) |> 
        withSpinnerCustom(height = 50)
    })
    
    output$budget <- renderReactable({
      budgetReactable(budget())
    }) 
    
  })
}
