box::use(
  bslib,
  dplyr,
  lubridate[as_datetime],
  purrr[is_empty],
  reactable[
    colDef,
    colFormat,
    getReactableState,
    reactable,
    reactableOutput,
    renderReactable,
  ],
  readr[
    read_csv,
    write_csv,
  ],
  shiny,
  shinyFeedback[showToast],
  shinyjs[click, disable, enable, reset],
  stringr[
    str_to_lower, 
  ],
  utils[download.file, ],
)

box::use(
  app/logic/constant,
  app/logic/db/get[getBankTransactions],
  app/logic/db/database[portalQuery],
  app/logic/db/logFunctions[logBankTransaction],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[approveTransaction, rejectTransaction],
  app/logic/player/playerChecks[
    hasActivePlayer,
  ],
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
    
    if (any(auth$usergroup |> is.null(), auth$suspended |> is.null())) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE, PLEASE LOG IN!"
      })
    } else if (isNonActiveForumUser(auth$usergroup, auth$suspended)) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ACCESS TO THIS PAGE"
      })
    } else if (!hasActivePlayer(auth$uid)) {
      output$ui <- shiny$renderUI({
        "YOU DO NOT HAVE ANY ACTIVE PLAYER, PLEASE CREATE ONE FIRST."
      })
    } else {
      
      #### OUTPUT UI ####
      output$ui <- shiny$renderUI({
        shiny$tagList(
          bslib$card(
            bslib$card_header(
              shiny$h3("Process Bank Transactions")
            ),
            bslib$card_body(
              shiny$h4("Instructions"),
              shiny$p("The table contains all unprocessed bank transactions. Mark the ones
                      that you want to process and click on approve or reject accordingly."),
              shiny$h5("Unprocessed transactions"),
              reactableOutput(ns("needApproval")),
              shiny$br(),
              shiny$br(),
              bslib$layout_column_wrap(
                width = 1 / 2,
                shiny$actionButton(ns("approve"), "Approve selected transactions"),
                shiny$actionButton(ns("reject"), "Reject selected transactions")
              )
            )
          ),
          bslib$card(
            bslib$card_header(
              shiny$h3("Historical Transactions")
            ),
            bslib$card_body(
              reactableOutput(ns("history"))
            )
          )
        )
        
      })
      
      #### REACTIVES ####
      
      unapprovedTransactions <- shiny$reactive({
        getBankTransactions(status = 0) |> 
          dplyr$select(
            !c(Status, `Approved By`)
          )
      }) |> 
        shiny$bindEvent(updated())
      
      approvedTransactions <- shiny$reactive({
        getBankTransactions(status = c(-1, 1)) |> 
          dplyr$mutate(
            Status = dplyr$if_else(Status == 1, "Approved", "Rejected")
          )
      }) |> 
        shiny$bindEvent(updated())
      
      
      #### OUTPUT SERVER ####
      output$needApproval <- renderReactable({
        if (unapprovedTransactions() |> is_empty()) {
          NULL
        } else {
          unapprovedTransactions() |> 
            dplyr$mutate(
              Time = as_datetime(Time, tz = "US/Pacific")
            ) |> 
            reactable(
              selection = "multiple",
              onClick = "select",
              pagination = FALSE,
              columns = 
                list(
                  pid = colDef(show = FALSE),
                  Time = 
                    colDef(format = colFormat(datetime = TRUE)),
                  Transaction = 
                    colDef(
                      style = function(value) {
                        list(color = dplyr$if_else(value < 0, constant$red, "white"))
                      },
                      format = colFormat(prefix = "$", separators = TRUE, digits = 0)
                    )
                )
            )
        }
      })
      
      output$history <- renderReactable({
        approvedTransactions() |> 
          dplyr$mutate(
            Time = as_datetime(Time, tz = "US/Pacific")
          ) |> 
          reactable(
            searchable = TRUE,
            columns = 
              list(
                pid = colDef(show = FALSE),
                Time = 
                  colDef(format = colFormat(datetime = TRUE)),
                Transaction = 
                  colDef(
                    style = function(value) {
                      list(color = dplyr$if_else(value < 0, constant$red, "white"))
                    },
                    format = colFormat(prefix = "$", separators = TRUE, digits = 0)
                  )
              )
          )
      })
      
      
      #### OBSERVERS ####
      shiny$observe({
        selected <- getReactableState("needApproval", "selected")
        shiny$req(selected)
        
        
        
        tryCatch({
          approveTransaction(unapprovedTransactions()[selected, ], uid = auth$uid)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "The selected transactions have successfully been APPROVED"
          )
          
          updated(updated() + 1)
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "Something went wrong, please reach out to Canadice"
          )
          
          message("Transaction failed, rolling back: ", e$message)
        })
      }) |> 
        shiny$bindEvent(
          input$approve
        )
      
      shiny$observe({
        selected <- getReactableState("needApproval", "selected")
        shiny$req(selected)
        
        tryCatch({
          rejectTransaction(unapprovedTransactions()[selected, ], uid = auth$uid)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "The selected transactions have successfully been REJECTED"
          )
          
          updated(updated() + 1)
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            "Something went wrong, please reach out to Canadice"
          )
        })
      }) |> 
        shiny$bindEvent(
          input$reject
        )
      
      
    }
  })
}
