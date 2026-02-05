box::use(
  bslib,
  dplyover[across2],
  dplyr[
    across,
    arrange,
    case_when,
    contains,
    ends_with,
    filter,
    full_join,
    group_by,
    if_else,
    intersect,
    left_join,
    mutate,
    n,
    relocate,
    rename_with,
    select,
    summarize,
    tibble,
    ungroup
  ],
  reactable[reactable, reactableOutput, renderReactable],
  shiny,
  shinyFeedback[showToast],
  shinyjs[disable, enable],
  stringr[str_remove_all, str_to_lower, str_to_title],
  tidyr[pivot_longer, replace_na],
)

box::use(
  app/logic/constant,
  app/logic/db/discord[sendAcademyIndexUpdate, sendIndexUpdate,],
  app/logic/db/get[getNextGameID, getSeasonalTotal],
  app/logic/db/login[isBoD, isFileworker],
  app/logic/import[importAcademyData, importGameData, parseFMdata],
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
      shiny$tabsetPanel(
        id = ns("tabs"),
        selected = input$tabs,
        shiny$tabPanel(
          "SSL League",
          shiny$tagList(
            shiny$h4("Upload and Update Game Statistics"),
            shiny$p(
              paste(
                "Export the Player Statistics for Export view from the
            League Observer Manager shortlist after the matchday you
            want to upload.",
                "In case you do not have the view for the League Observer
            Manager, download and import the view via",
                shiny$tags$a("this link.",
                             href = "https://drive.google.com/open?id=1b4yz5gkXN6BFSvDBigL3Tp2pUBbu-PJ3&usp=drive_fs",
                             target = "_blank"
                ),
                "The view should be exported as an .HTML file and make
            sure to scroll through every player in the shortlist as
            FM only exports players it has 'seen'."
              ) |>
                shiny$HTML()
            ),
            bslib$layout_column_wrap(
              width = NULL,
              style = bslib$css(grid_template_columns = "1fr 2fr"),
              shiny$div(
                shiny$fileInput(
                  inputId = ns("fm"),
                  label = "Upload the exported view ",
                  accept = ".html"
                ),
                shiny$selectInput(
                  inputId = ns("season"),
                  label = "Which season is the view from?",
                  choices = 1:constant$currentSeason$season,
                  selected = constant$currentSeason$season
                ),
                shiny$selectInput(
                  inputId = ns("league"),
                  label = "Which file/league is the view from?",
                  choices = 
                    c("Major" = 1, "Minor" = 2, "Cup" = 0, "WSFC" = 5)
                )
              ),
              shiny$div(
                if (filePath() |> is.null()) {
                  NULL
                } else {
                  shiny$req(input$fm)
                  shiny$actionButton(
                    ns("uploadData"),
                    label = "Upload game data"
                  )
                }
              )
            ),
            bslib$layout_column_wrap(
              width = NULL,
              style = bslib$css(grid_template_columns = "2fr 1fr"),
              shiny$div(
                shiny$p("If the data is correctly imported, the table should show
                  90 minutes played on average for outfield and goalkeepers."),
                shiny$h5("Checking data"),
                reactableOutput(ns("outputMinutes")),
                shiny$h5("Players per team"),
                reactableOutput(ns("outputPlayers")),
                shiny$h5("Keeper Data"),
                reactableOutput(ns("keeperCheck")),
                shiny$h5("Outfield Data"),
                reactableOutput(ns("outfieldCheck"))
              ),
              ""
            )
          )
        ),
        shiny$tabPanel(
          "SSL Academy",
          shiny$tagList(
            shiny$h4("Upload and Update Academy Season Statistics"),
            shiny$p(
              paste(
                "Export the Player Statistics for Export view from the
            League Observer Manager shortlist after the matchday you
            want to upload.",
                "In case you do not have the view for the League Observer
            Manager, download and import the view via",
                shiny$tags$a("this link.",
                             href = "https://drive.google.com/open?id=1b4yz5gkXN6BFSvDBigL3Tp2pUBbu-PJ3&usp=drive_fs",
                             target = "_blank"
                ),
                "The view should be exported as an .HTML file and make
            sure to scroll through every player in the shortlist as
            FM only exports players it has 'seen'."
              ) |>
                shiny$HTML()
            ),
            bslib$layout_column_wrap(
              width = NULL,
              style = bslib$css(grid_template_columns = "1fr 2fr"),
              shiny$div(
                shiny$fileInput(
                  inputId = ns("fmAcademy"),
                  label = "Upload the exported view ",
                  accept = ".html"
                ),
                shiny$selectInput(
                  inputId = ns("seasonAcademy"),
                  label = "Which season is the view from?",
                  choices = 1:constant$currentSeason$season,
                  selected = constant$currentSeason$season
                )
              ),
              shiny$div(
                if (filePathAcademy() |> is.null()) {
                  NULL
                } else {
                  shiny$req(input$fmAcademy)
                  shiny$actionButton(
                    ns("uploadDataAcademy"),
                    label = "Upload game data"
                  )
                }
              )
            ),
            bslib$layout_column_wrap(
              width = NULL,
              style = bslib$css(grid_template_columns = "2fr 1fr"),
              shiny$div(
                shiny$p("If the data is correctly imported, the table should show
                  90 minutes played on average for outfield and goalkeepers."),
                shiny$h5("Keeper Data"),
                reactableOutput(ns("keeperCheckAcademy")),
                shiny$h5("Outfield Data"),
                reactableOutput(ns("outfieldCheckAcademy"))
              ),
              ""
            )
          )
        )
      )
    })

    #### OUTPUT SERVER ####

    ## REACTIVES
    filePath <- shiny$reactive({
      file <- input$fm

      enable("uploadData")

      file$datapath
    })
    
    filePathAcademy <- shiny$reactive({
      file <- input$fmAcademy
      
      enable("uploadDataAcademy")
      
      file$datapath
    })

    nextGames <- shiny$reactive({
      shiny$req(input$season)

      getNextGameID(input$season, league = input$league)
    }) |> 
      shiny$bindEvent(filePath())

    processedGame <- shiny$reactive({
      currentSave <- parseFMdata(filePath()) |> 
        select(!c(CA, UID))

      keeperTotals <- getSeasonalTotal(season = input$season, outfield = FALSE, league = input$league)
      outfieldTotals <- getSeasonalTotal(season = input$season, league = input$league)

      current <-
        currentSave |>
        rename_with(
          .cols = !c(Acc:`Right Foot`, Position, Won, Lost, Drawn),
          .fn = function(x) {
            intersect(
              x |> str_to_lower(),
              c(colnames(outfieldTotals), colnames(keeperTotals)) |>
                unique()
            )
          }
        ) |>
        relocate(
          c(colnames(outfieldTotals), colnames(keeperTotals)) |>
            unique()
        ) |>
        rename_with(str_to_lower)

      splitKeeper <-
        current |>
        filter(position == "GK") |>
        select(colnames(keeperTotals)) |>
        full_join(
          keeperTotals,
          by = c("name", "club")
        ) |>
        group_by(name, club) |>
        mutate(
          across2(
            .xcols = ends_with(".x"),
            .ycols = ends_with(".y"),
            .fns =
              list(
                diff = ~ sum(.x, -.y, na.rm = TRUE)
              ),
            .names = "{xcol}"
          )
        ) |>
        select(!contains(".y")) |>
        rename_with(~ str_remove_all(.x, "\\.x")) |>
        filter(`minutes played` > 0) |>
        left_join(
          keeperTotals |>
            select(name, club, `average rating`, `xsave%`, apps),
          by = c("name", "club"),
          suffix = c("day", "season")
        ) |>
        group_by(name) |>
        mutate(
          `average ratingday` =
            case_when(
              is.na(`average ratingseason`) | `average ratingseason` == 0 ~ `average ratingday`,
              TRUE ~ (
                (`average ratingday` + `average ratingseason`) *
                  (`appsseason` + 1) -
                  `average ratingseason` * `appsseason`
              )
            ) |> round(2),
          `save%` = ((`saves parried` + `saves held` + `saves tipped`)/
            (`saves parried` + `saves held` + `saves tipped` + conceded)) |>
            round(4) * 100,
          `xsave%day` =
            case_when(
              is.na(`xsave%season`) | `xsave%season` == 0 ~ `xsave%day`,
              TRUE ~ (
                ((`xsave%day` + `xsave%season`) -
                  `xsave%season`/2) * 2
              )
            ) |> round(2)
        ) |>
        select(
          !contains("season"),
          `average rating` = `average ratingday`,
          `xsave%` = `xsave%day`,
          `apps` = `appsday`
        ) |>
        mutate(
          apps =
            case_when(
              `minutes played` > 45 ~ 1,
              TRUE ~ 0.5
            )
        ) |>
        ungroup() |>
        left_join(
          nextGames(),
          by = c("club" = "team")
        ) |>
        mutate(
          across(
            !(name:club),
            ~ replace_na(.x, 0)
          )
        )

      splitOutfield <-
        current |>
        select(colnames(outfieldTotals)) |>
        full_join(
          outfieldTotals,
          by = c("name", "club")
        ) |>
        group_by(name, club) |>
        mutate(
          across2(
            .xcols = ends_with(".x"),
            .ycols = ends_with(".y"),
            .fns =
              list(
                diff = ~ sum(.x, -.y, na.rm = TRUE)
              ),
            .names = "{xcol}"
          )
        ) |>
        select(!contains(".y")) |>
        rename_with(~ str_remove_all(.x, "\\.x")) |>
        filter(`minutes played` > 0) |>
        left_join(
          outfieldTotals |>
            select(name, club, `average rating`, apps),
          by = c("name", "club"),
          suffix = c("day", "season")
        ) |>
        group_by(name) |>
        mutate(
          `average ratingday` =
            case_when(
              is.na(`average ratingseason`) | `average ratingseason` == 0 ~ `average ratingday`,
              TRUE ~ (
                (`average ratingday` + `average ratingseason`) *
                  (`appsseason` + 1) -
                  `average ratingseason` * `appsseason`
              )
            ) |> round(2),
          `pass%` = (`successful passes` |> as.numeric()/`attempted passes` |> as.numeric()) |> round(4) * 100,
          `header%` = (`successful headers` |> as.numeric()/`attempted headers` |> as.numeric()) |> round(4) * 100,
          `cross%` = (`successful crosses` |> as.numeric()/`attempted crosses` |> as.numeric()) |> round(4) * 100,
          `tackle%` = (`tackles won` |> as.numeric()/`attempted tackles` |> as.numeric()) |> round(4) * 100,
          clearances = if_else(is.na(clearances), 0, clearances)
        ) |>
        select(
          !contains("season"),
          `average rating` = `average ratingday`,
          `apps` = `appsday`
        ) |>
        mutate(
          apps =
            case_when(
              `minutes played` > 45 ~ 1,
              TRUE ~ 0.5
            )
        ) |>
        ungroup() |>
        left_join(
          nextGames(),
          by = c("club" = "team")
        ) |>
        left_join(
          current |>
            select(name, club, position, acc:wor),
          by = c("name", "club")
        ) |>
        relocate(
          c(position, acc:wor),
          .after = club
        ) |>
        mutate(
          across(
            !(name:position),
            ~ replace_na(.x, 0)
          )
        )


      list(
        k = splitKeeper,
        o = splitOutfield,
        checks = tibble(
          `Outfield Minutes` =
            (sum(splitOutfield$`minutes played`)/
              length(splitOutfield$`club` |>
                unique())/11) |>
              round(1),
          `Keeper Minutes` =
            (sum(splitKeeper$`minutes played`)/
              length(splitKeeper$`club` |>
                unique())) |>
              round(1),
          `Red Cards` = splitOutfield$`red cards` |> sum()
        ),
        playersPerTeam =
          splitOutfield |>
            select(name, club) |>
            pivot_longer(name) |>
            group_by(club) |>
            summarize(n = n())
      )
    }) |>
      shiny$bindEvent(input$fm)
    
    processedGameAcademy <- shiny$reactive({
      shiny$req(filePathAcademy())
      
      current <- parseFMdata(filePathAcademy()) |> 
        rename_with(str_to_lower)
      
      splitKeeper <- 
        current |>
        filter(position == "GK", !is.na(`minutes played`)) |> 
        select(name:`minutes played`, `average rating`:`player of the match`, won:`xg prevented`, `save%`) |> 
        mutate(season = input$seasonAcademy) |> 
        relocate(season) |> 
        mutate(
          across(
            !(name:position),
            ~ replace_na(.x, 0)
          )
        )
      
      splitOutfield <- 
        current |> 
        filter(!is.na(`minutes played`)) |> 
        select(name:`attempted presses`, `pass%`:`cross%`) |> 
        mutate(season = input$season) |> 
        relocate(season) |> 
        mutate(
          across(
            !(name:position),
            ~ replace_na(.x, 0)
          )
        )
      
      list(
        k = splitKeeper,
        o = splitOutfield
      )
    }) |> 
      shiny$bindEvent(input$fmAcademy)

    ## OUTPUT
    output$outputMinutes <- renderReactable({
      processedGame()$checks |>
        reactable()
    })

    output$outputPlayers <- renderReactable({
      processedGame()$playersPerTeam |>
        rename_with(~ str_to_title(.x)) |>
        reactable()
    })

    output$outfieldCheck <- renderReactable({
      processedGame()$o |>
        relocate(gid) |>
        arrange(gid) |>
        reactable()
    })
    
    output$outfieldCheckAcademy <- renderReactable({
      processedGameAcademy()$o |>
        reactable()
    })

    output$keeperCheck <- renderReactable({
      processedGame()$k |>
        relocate(gid) |>
        arrange(gid) |>
        reactable()
    })
    
    output$keeperCheckAcademy <- renderReactable({
      processedGameAcademy()$k |>
        reactable()
    })

    #### OBSERVER ####
    shiny$observe({
      shiny$req(input$fm)
      
      tryCatch({
        disable("uploadData")
        
        importGameData(processedGame())
        
        sendIndexUpdate(input$season)
        
        showToast(
          .options = constant$myToastOptions,
          "success",
          "You have successfully uploaded the recent matchday!"
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
        
        message("Transaction failed, rolling back: ", e$message)
      })
      
    }) |>
      shiny$bindEvent(input$uploadData)
    
    shiny$observe({
      shiny$req(filePathAcademy())
      
      tryCatch({
        
        disable("uploadDataAcademy")
        
        importAcademyData(processedGameAcademy())
        
        sendAcademyIndexUpdate(input$season)
        
        showToast(
          .options = constant$myToastOptions,
          "success",
          "You have successfully uploaded the recent matchday!"
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
        
        message("Transaction failed, rolling back: ", e$message)
      })
    }) |>
      shiny$bindEvent(input$uploadDataAcademy)
  })
}
