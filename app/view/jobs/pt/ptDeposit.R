box::use(
  bslib,
  dplyr,
  reactable[
    reactable,
    reactableOutput,
    renderReactable,
  ],
  readr[
    problems,
    read_csv,
    write_csv,
  ],
  shiny,
  shinyFeedback[showToast],
  shinyjs[click, disable, disabled, enable, reset],
  stringr[
    str_to_lower, 
  ],
  utils[download.file, ],
)

box::use(
  app/logic/constant,
  app/logic/db/database[portalQuery],
  app/logic/db/discord[sendGradedTPE],
  app/logic/db/login[isNonActiveForumUser],
  app/logic/db/updateFunctions[updateTPE],
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
            shiny$h3("PT Deposit")
          ),
          bslib$card_body(
            bslib$layout_column_wrap(
              width = 1 / 2,
              shiny$div(
                shiny$h5("Instructions"),
                shiny$p(
                  paste0(
                    "The .csv file should contain the username ('user'), the 
                    tpe gained ('tpe') and the source ('source'). The source column
                    does not need to be filled out as you will be able to manually
                    enter one single source for all empty rows in this tool.
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
                  src = "https://i.imgur.com/0fy7ucf.png"
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
                label = "If no source value is given for specific entries in the file, enter it here:"
              ) |> 
                disabled()
            ),
            shiny$h4("Check the processed deposit"),
            shiny$p("The processed file is shown below. Red highlighted rows
                    indicate that an active player has not been found for the given username.
                    The spelling is case sensitive so check the capitalization.
                    Any row with no source specified can be input using the text input."),
            reactableOutput(ns("checkImport"))
          )
        ),
        shiny$actionButton(
          ns("confirmDeposit"),
          label = "Confirm deposit",
          class = "primary-button"
        ) |> 
          disabled() |> 
          shiny$div(class = "frozen-bottom"),
        shiny$div(style = "min-height:100px;")
      )
      
    })
    
    #### REACTIVES ####
    ptDeposit <- shiny$reactive({
      shiny$req(input$depositFile)
      
      file <- input$depositFile
      
      data <- read_csv(
        file = file$datapath,
        show_col_types = FALSE,
        col_types = "cdc"
      )
      
      if (problems(data) |> nrow() != 0) {
        showToast(
          .options = constant$sslToastOptions,
          "error",
          "Something is wrong with the submitted file. Please check it against the
          instructions and try again."
        )
        
        NULL
      } else if (all(c("username", "tpe", "source") %in% 
          (colnames(data) |> str_to_lower()))) {
        
        colnames(data) <- colnames(data) |> str_to_lower()
        
        ## Gets player ids for all the active players in the list
        pids <- 
          portalQuery(
            "SELECT pid, name, username
              FROM allplayersview
              WHERE status_p = 1 AND LOWER(username) IN ({usernames*});",
            usernames = str_to_lower(data$username)
          )
        
        enable("depositSource")
        enable("confirmDeposit")
        
        data |> 
          dplyr$mutate(
            userLower = str_to_lower(username)
          ) |> 
          dplyr$select(!username) |> 
          dplyr$left_join(
            pids |> 
              dplyr$mutate(
                userLower = str_to_lower(username)
              ),
            by = "userLower"
          ) |> 
          dplyr$mutate(
            pid = dplyr$if_else(is.na(pid), -99, pid)
          ) |> 
          dplyr$select(!userLower) |> 
          dplyr$relocate(c(username, name, pid))
        
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
      if (ptDeposit() |> is.null()) {
        NULL
      } else {
        
        data <- ptDeposit() |> 
          dplyr$mutate(
            source = dplyr$if_else(
              is.na(source),
              input$depositSource,
              source
            ),
            logic = 
              (pid < 0) |
              (source |> nchar() > 255) |
              (
                (source |> is.na()) &
                  (input$depositSource |> nchar() > 255)
              )
          )
        
        data |> 
          dplyr$select(!logic) |> 
          reactable(
            pagination = FALSE,
            rowStyle = function(index) {
              if (data[index, "logic"] == TRUE) {
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
        ptDeposit() |> 
          dplyr$filter(pid == -99) |> 
          dplyr$select(!c(pid, name)) |> 
          write_csv(file, na = "")
      }
    )
    
    output$downloadTemplate <- shiny$downloadHandler(
      filename = function() {
        "PT Deposit Template.csv"
      },
      content = function(file) {
        url <- 
          "https://raw.githubusercontent.com/canadice/ssl-index/main/SSL-Index/ptDepositTemplate.csv"
        download.file(url, destfile = file)
      }
    )
    
    
    #### OBSERVERS ####
    shiny$observe({
      disable("confirmDeposit")
      
      processed <- 
        ptDeposit() |> 
        dplyr$mutate(
          source = dplyr$if_else(
            is.na(source),
            input$depositSource,
            source
          )
        ) |> 
        dplyr$filter(pid != -99)  
      
      unProcessed <- 
        ptDeposit() |> 
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
          updateTPE(
            uid = auth$uid,
            pids = processed$pid,
            tpe = processed |> dplyr$select(source, tpe)
          )
          
          updated(updated() + 1)
          
          showToast(
            .options = constant$sslToastOptions,
            "success",
            "You have successfully made a deposit!"
          )
          
          sendGradedTPE(data = processed)
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
