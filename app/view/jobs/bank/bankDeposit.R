box::use(
  bslib,
  dplyr,
  reactable[
    colDef,
    colFormat,
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
  app/logic/db/database[portalQuery],
  app/logic/db/logFunctions[logBankTransaction],
  app/logic/db/login[isNonActiveForumUser],
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
    
    #### OUTPUT UI ####
    output$ui <- shiny$renderUI({
      shiny$tagList(
        bslib$card(
          bslib$card_header(
            shiny$h3("Bank Deposit")
          ),
          bslib$card_body(
            bslib$layout_column_wrap(
              width = 1 / 2,
              shiny$div(
                shiny$h5("Instructions"),
                shiny$p(
                  paste0(
                    "The .csv file should contain the player name ('player'), the 
                    money gained ('amount') and the source ('source'). 
                    The encoding of the file should be UTF-8. If you are not sure how 
                    to check this, follow the instructions in this ", 
                    shiny$a(
                      "link", 
                      href = "https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8"
                    ),
                    '. The file should not have any other symbols like $ or " around the 
                    amount or names. If you edit and export from Excel this will most
                    likely be the case. It is strongly recommended to open in Notepad or
                    Notepad++ (or equivalent) before uploading.'
                  ) |> 
                    shiny$HTML()
                )
              ),
              shiny$div(
                shiny$p("The template file should be populated like this:"),
                shiny$tags$img(
                  src = "https://i.imgur.com/pEZymWd.png"
                ),
                shiny$br(),
                shiny$br(),
                shiny$downloadButton(
                  ns("downloadTemplate"),
                  label = "Download the template file"
                ),
                shiny$downloadButton(
                  ns("downloadData"),
                  label = "Fake", 
                  style = "visibility: hidden;"
                ) 
              ),
              shiny$fileInput(
                inputId = ns("depositFile"),
                label = "Upload a comma (,) separated .csv file:",
                accept = ".csv"
              ) |> 
                shiny$div(id = ns("fileInput")),
              shiny$textInput(
                inputId = ns("depositSource"),
                label = "If no source is given in the file, enter it here:"
              )
            ),
            shiny$h4("Check the processed deposit"),
            shiny$p("The processed file is shown below. Red highlighted rows
                    indicate that the player is not found with the current spelling
                    or the that source is too long (limit 255 characters).
                    
                    Any row with no source specified can be input using the text input."),
            reactableOutput(ns("checkImport"))
          )
        ),
        shiny$actionButton(
          ns("confirmDeposit"),
          label = "Confirm deposit",
          class = "primary-button"
        ) |> 
          shiny$div(class = "frozen-bottom"),
        shiny$div(style = "min-height:100px;")
      )
      
    })
    
    #### REACTIVES ####
    bankDeposit <- shiny$reactive({
      shiny$req(input$depositFile)
      
      file <- input$depositFile
      
      data <- read_csv(
        file = file$datapath,
        show_col_types = FALSE
      )
      
      if (all(c("player", "amount", "source") %in% 
          (colnames(data) |> str_to_lower()))) {
        
        colnames(data) <- colnames(data) |> str_to_lower()
        
        ## Gets player ids for all the active players in the list
        pids <- 
          portalQuery(
            "SELECT pid, name
            FROM allplayersview
            WHERE name IN ({playerList*}) AND status_p = 1;",
            playerList = data$player
          )
        
        enable("confirmDeposit")
        
        data |> 
          dplyr$left_join(
            pids,
            by = c("player" = "name")
          ) |> 
          dplyr$mutate(
            pid = dplyr$if_else(is.na(pid), -99, pid)
          )
        
      } else {
        showToast(
          .options = constant$sslToastOptions,
          "error",
          "The file does not contain the required headers. Please check the instructions."
        )
        
        NULL
      }
    })
    
    #### OUTPUT SERVER ####
    output$checkImport <- renderReactable({
      if (bankDeposit() |> is.null()) {
        NULL
      } else {
        bankDeposit() |> 
          dplyr$mutate(
            source = dplyr$if_else(
              is.na(source),
              input$depositSource,
              source
            ) 
          ) |> 
          reactable(
            pagination = FALSE,
            columns = 
              list(
                amount = 
                  colDef(
                    format = 
                      colFormat(
                        digits = 0, 
                        separators = TRUE, 
                        currency = "USD"
                        )
                    )
              ),
            rowStyle = function(index) {
              if (bankDeposit()[index, "pid"] < 0 | 
                  (bankDeposit()[index, "source"] |> nchar() > 255) |
                  (
                    (bankDeposit()[index, "source"] |> is.na()) & 
                    (input$depositSource |> nchar() > 255)
                  )
              ) {
                list(background = constant$red, color = "white")
              }
            }
          )
      }
    })
    
    output$downloadData <- shiny$downloadHandler(
      filename = function() {
        paste("unprocessed", input$depositFile$name)
      },
      content = function(file) {
        bankDeposit() |> 
          dplyr$filter(pid == -99) |> 
          dplyr$select(!pid) |> 
          write_csv(file, na = "")
      }
    )
    
    output$downloadTemplate <- shiny$downloadHandler(
      filename = function() {
        "Bank Deposit Template.csv"
      },
      content = function(file) {
        url <- "https://raw.githubusercontent.com/canadice/ssl-index/main/SSL-Index/bankDepositTemplate.csv"  
        download.file(url, destfile = file)
      }
    )
    
    
    #### OBSERVERS ####
    shiny$observe({
      disable("confirmDeposit")
      
      processed <- 
        bankDeposit() |> 
        dplyr$mutate(
          source = dplyr$if_else(
            is.na(source),
            input$depositSource,
            source
          )
        ) |> 
        dplyr$filter(pid != -99)  
      
      unProcessed <- 
        bankDeposit() |> 
        dplyr$filter(pid == -99)
      
      if (nrow(unProcessed) > 0) {
        showToast(
          .options = constant$sslToastOptions,
          "warning",
          "At least one player in the submitted csv has not been matched to an active player.
          A filtered file has been downloaded for manual processing."
        )
        
        click("downloadData")
      }
      
      if (nrow(processed) <= 0) {
        showToast(
          .options = constant$myToastOptions,
          "error",
          "You have no deposits that can be processed."
        )
      } else {
        
        
        tryCatch({
          logBankTransaction(
            uid = auth$uid,
            pid = processed$pid,
            source = processed$source,
            transaction = processed$amount,
            status = 0
          )
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully made a deposit!"
          )
        }, error = function(e) {
          showToast(
            .options = constant$sslToastOptions,
            "error",
            paste(
              "Something is wrong, please notify the BoD with the 
                following error message: \n",
              e$message
            )
          )
          
          message("Error making a deposit: ", e)
          
        })
      }
      
      reset("fileInput")
        
    }) |> 
      shiny$bindEvent(
        input$confirmDeposit
      )
  })
}
