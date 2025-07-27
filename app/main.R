box::use(
  cachem,
  shiny,
  shiny.router[change_page, route, router_server, router_ui],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
  stringr[str_detect, str_remove],
)

box::use(
  app/logic/db/login[
    isBankerAccountant,
    isBoD,
    isBoDIntern,
    isFileworker,
    isManager,
    isPT,
  ],
  app/logic/player/playerChecks[navigationCheck],
  app/view/bank/myBank,
  app/view/index/academyIndex,
  app/view/index/careerRecords,
  app/view/index/leagueIndex,
  app/view/index/schedule,
  app/view/index/standings,
  app/view/jobs/bank/bankDeposit,
  app/view/jobs/bank/bankProcess,
  app/view/jobs/bod/approvePlayer,
  app/view/jobs/bod/assignManager,
  app/view/jobs/bod/editPlayer,
  app/view/jobs/filework/export,
  app/view/jobs/filework/import,
  app/view/jobs/filework/scheduleEdit,
  app/view/jobs/manager/rosterOverview,
  app/view/jobs/pt/ptDeposit,
  app/view/navigationBar,
  app/view/player/createPlayer,
  app/view/player/myPlayer,
  app/view/player/playerUpdate,
  app/view/tracker/draftclass,
  app/view/tracker/organization,
  app/view/tracker/player,
  app/view/tracker/playerSearch,
  app/view/welcome,
)

shiny$shinyOptions(cache = cachem$cache_mem(max_size = 500e6, max_age = 8 * 60 * 60))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$bootstrapPage(
    useShinyFeedback(), # include shinyFeedback
    useShinyjs(), # include shinyjs
    title = "SSL Portal",
    navigationBar$ui(ns("nav")),
    router_ui(
      route("/", welcome$ui(ns("welcome"))),
      route("index/academy", academyIndex$ui(ns("academy"))),
      route("index/", leagueIndex$ui(ns("league"))),
      route("index/records", careerRecords$ui(ns("records"))),
      route("index/schedule", schedule$ui(ns("schedule"))),
      route("index/standings", standings$ui(ns("standings"))),
      route("tracker/draftclass", draftclass$ui(ns("draftclass"))),
      route("tracker/player", player$ui(ns("player"))),
      route("tracker/organization", organization$ui(ns("organization"))),
      route("search", playerSearch$ui(ns("search"))),
      route("myBank", myBank$ui(ns("myBank"))),
      route("bank/deposit", bankDeposit$ui(ns("bankDeposit"))),
      route("bank/process", bankProcess$ui(ns("bankProcess"))),
      route("myPlayer/", myPlayer$ui(ns("myPlayer"))),
      route("myPlayer/update", playerUpdate$ui(ns("update"))),
      route("myPlayer/reroll", playerUpdate$ui(ns("reroll"))),
      route("myPlayer/redistribute", playerUpdate$ui(ns("redist"))),
      route("myPlayer/regress", playerUpdate$ui(ns("regress"))),
      route("createPlayer", createPlayer$ui(ns("create"))),
      route("filework/export", export$ui(ns("export"))),
      route("filework/import", import$ui(ns("import"))),
      route("filework/scheduleEdit", scheduleEdit$ui(ns("scheduleEdit"))),
      route("pt/deposit", ptDeposit$ui(ns("ptDeposit"))),
      route("bod/manager", assignManager$ui(ns("assignManager"))),
      route("bod/approve", approvePlayer$ui(ns("approvePlayer"))),
      route("bod/edit", editPlayer$ui(ns("editPlayer"))),
      route("organization/overview", rosterOverview$ui(ns("rosterOverview"))),
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    router_server("/")

    ## Reactives
    resAuth <- shiny$reactiveValues(
      uid = NULL,
      username = NULL,
      usergroup = NULL,
      suspended = 0
    )

    # Adds all authentication list to a reactive object
    authOutput <- shiny$reactive({
      shiny$reactiveValuesToList(resAuth)
    })
    
    ## Defines a reactive value that tracks if portal updates are 
    ## made that require re-loading assets
    updated <- shiny$reactiveVal(0)
    
    navigationBar$server("nav", auth = authOutput, resAuth = resAuth, updated = updated)
    
    playerSearch$server("search")

    ## In order to load pages as they are clicked ONCE this is needed
    loadedServer <-
      shiny$reactiveValues(
        create = FALSE, player = FALSE, index = FALSE, playerUpdate = FALSE,
        playerReroll = FALSE, playerRedist = FALSE, playerRegress = FALSE,
        myPlayer = FALSE, import = FALSE, standings = FALSE, 
        schedule = FALSE, academy = FALSE, 
        myBank = FALSE, welcome = FALSE, records = FALSE,
        playerPages = FALSE, contractProcess = FALSE,
        tradeProcess = FALSE, ptDeposit = FALSE,
        bankDeposit = FALSE, bankProcess = FALSE, scheduleEdit = FALSE,
        managerTeam = FALSE,
        assignManager = FALSE, approvePlayer = FALSE, editPlayer = FALSE,
        rosterOverview = FALSE, 
        export = FALSE, organization = FALSE, draftclass = FALSE, 
        nationTracker = FALSE,
        positionTracker = FALSE, main = FALSE
      )
    
    ## Observer that checks the current page and loads the server for the page ONCE
    shiny$observe({
      current <- str_remove(session$clientData$url_hash,
        pattern = "#!/"
      )
      
      if (current == "" & !loadedServer$main) {
        
        welcome$server("welcome", usergroup = authOutput()$usergroup)
        loadedServer$main <- TRUE
        
      } else if (current == "index/records" & !loadedServer$records) {
        
        careerRecords$server("records")
        loadedServer$records <- TRUE
        
      } else if (current == "index/" & !loadedServer$index) {
        
        leagueIndex$server("league")
        loadedServer$index <- TRUE
        
      } else if (current == "index/standings" & !loadedServer$standings) {
        
        standings$server("standings", updated)
        loadedServer$standings <- TRUE
        
      } else if (current == "index/schedule" & !loadedServer$schedule) {
        
        schedule$server("schedule", updated)
        loadedServer$schedule <- TRUE
        
      } else if (current == "index/academy" & !loadedServer$academy) {
       
        academyIndex$server("academy")
        loadedServer$academy <- TRUE
        
      } else if (current == "tracker/organization" & !loadedServer$organization) {
        
        organization$server("organization")
        loadedServer$organization <- TRUE
        
      } else if (current == "tracker/draftclass" & !loadedServer$draftclass) {
        
        draftclass$server("draftclass")
        loadedServer$draftclass <- TRUE
        
      } else if (current |> str_detect("tracker/player") & !loadedServer$player) {
        
        player$server("player", updated = updated)
        loadedServer$player <- TRUE
      
      } else if (current == "myPlayer/") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$myPlayer) {
            myPlayer$server("myPlayer", auth = authOutput(), updated = updated)
            
            loadedServer$myPlayer <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "myBank") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$myBank) {
            myBank$server(
              "myBank",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$myBank <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "filework/export") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$export &
              any(
                isFileworker(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
            ) {
            export$server(
              "export",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$export <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "filework/import") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$import & 
              any(
                isFileworker(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            import$server(
              "import",
              auth    = authOutput(),
              updated = updated()
            )
            loadedServer$import <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "myPlayer/update") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$playerUpdate) {
            playerUpdate$server(
              "update",
              auth    = authOutput(),
              updated = updated,
              type    = "update"
            )
            loadedServer$playerUpdate <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "myPlayer/reroll") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$playerReroll) {
            playerUpdate$server(
              "reroll",
              auth    = authOutput(),
              updated = updated,
              type    = "reroll"
            )
            loadedServer$playerReroll <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "myPlayer/redistribute") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$playerRedist) {
            playerUpdate$server(
              "redist",
              auth    = authOutput(),
              updated = updated,
              type    = "redistribution"
            )
            loadedServer$playerRedist <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "myPlayer/regress") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$playerRegress) {
            playerUpdate$server(
              "regress",
              auth    = authOutput(),
              updated = updated,
              type    = "regression"
            )
            loadedServer$playerRegress <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "createPlayer" & !loadedServer$create) { 
        
        createPlayer$server(
          "create", 
          auth = authOutput(), 
          updated = updated
        ) 
        
        loadedServer$create <- TRUE 
        
      } else if (current == "bank/deposit") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$bankDeposit & 
              any(
                isBankerAccountant(authOutput()$usergroup),
                isManager(authOutput()$usergroup),
                isPT(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            bankDeposit$server(
              "bankDeposit",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$bankDeposit <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "bank/process") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$bankProcess & 
              any(
                isBankerAccountant(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            bankProcess$server(
              "bankProcess",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$bankProcess <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "filework/scheduleEdit") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$scheduleEdit & 
              any(
                isFileworker(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            scheduleEdit$server(
              "scheduleEdit",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$scheduleEdit <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "pt/deposit") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$ptDeposit & 
              any(
                isPT(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            ptDeposit$server(
              "ptDeposit",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$ptDeposit <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "bod/manager") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$assignManager & 
              any(
                isManager(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            assignManager$server(
              "assignManager",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$assignManager <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "bod/approve") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$approvePlayer & 
              any(
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            approvePlayer$server(
              "approvePlayer",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$approvePlayer <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "bod/edit") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$editPlayer & 
              any(
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            editPlayer$server(
              "editPlayer",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$editPlayer <- TRUE
          }
        } else {
          change_page("")
        }
        
      } else if (current == "organization/overview") {
        
        if (navigationCheck(authOutput())) {
          if (!loadedServer$rosterOverview & 
              any(
                isManager(authOutput()$usergroup),
                isBoD(authOutput()$usergroup),
                isBoDIntern(authOutput()$usergroup)
              )
          ) {
            rosterOverview$server(
              "rosterOverview",
              auth    = authOutput(),
              updated = updated
            )
            loadedServer$rosterOverview <- TRUE
          }
        } else {
          change_page("")
        }
        
      }
      
    }) |>
      shiny$bindEvent(session$clientData$url_hash)
    
    # Reset all loadedServer flags whenever the auth object updates
    shiny$observe({
      # ensure authOutput() has a value
      shiny$req(authOutput())
      
      # grab all flag names, then set each to FALSE
      flags <- shiny$reactiveValuesToList(loadedServer)
      for (flagName in names(flags)) {
        loadedServer[[flagName]] <- FALSE
      }
    }) |> 
      shiny$bindEvent(
        authOutput(), 
        ignoreInit = TRUE  # skip on module startup
      )
    
    
  })
}
