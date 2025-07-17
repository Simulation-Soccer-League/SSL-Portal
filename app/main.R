box::use(
  cachem,
  shiny,
  shiny.router[route, router_server, router_ui],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
  stringr[str_detect, str_remove],
)

box::use(
  app/view/navigationBar,
  app/view/bank/myBank,
  app/view/index/academyIndex,
  app/view/index/careerRecords,
  app/view/index/leagueIndex,
  app/view/index/schedule,
  app/view/index/standings,
  app/view/jobs/bank/bankDeposit,
  app/view/jobs/bank/bankProcess,
  app/view/jobs/filework/export,
  app/view/jobs/filework/import,
  app/view/player/createPlayer,
  app/view/player/myPlayer,
  app/view/player/playerUpdate,
  app/view/tracker/draftclass,
  app/view/tracker/organization,
  app/view/tracker/player,
  app/view/tracker/playerSearch,
  app/view/welcome,
)

shiny$shinyOptions(cache = cachem$cache_mem(max_size = 500e6, max_age = 8*60*60))

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
      route("filework/import", import$ui(ns("import")))
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
    
    ## Defines a reactive value that tracks if portal updates are made that require re-loading assets
    updated <- shiny$reactiveVal(0)
    
    navigationBar$server("nav", auth = authOutput, resAuth = resAuth, updated = updated)
    
    welcome$server("welcome", usergroup = authOutput()$usergroup)

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
        tradeProcess = FALSE, playerEdit = FALSE, submitPT = FALSE,
        bankDeposit = FALSE, bankProcess = FALSE,
        managerTeam = FALSE,
        assignManager = FALSE, bodoverview = FALSE, export = FALSE,
        organization = FALSE, draftclass = FALSE, nationTracker = FALSE,
        positionTracker = FALSE
      )
    
    ## TODO: MIGHT NEED TO CHANGE SO THAT ANY PAGE TIED TO A LOGIN UPDATES WHEN SOMEONE LOGS IN/OUT
    ## CANNOT LOAD EVERY TIME THE PAGE LOADS AS THAT WOULD GENERATE MULTIPLE OBSERVERS

    ## Observer that checks the current page and loads the server for the page ONCE
    shiny$observe({
      current <- str_remove(session$clientData$url_hash,
        pattern = "#!/"
      )
      
      if (current == "index/records" & !loadedServer$records) {
        
        careerRecords$server("records")
        loadedServer$records <- TRUE
        
      } else if (current == "index/" & !loadedServer$index) {
        
        leagueIndex$server("league")
        loadedServer$index <- TRUE
        
      } else if (current == "index/standings" & !loadedServer$standings) {
        
        standings$server("standings")
        loadedServer$standings <- TRUE
        
      } else if (current == "index/schedule" & !loadedServer$schedule) {
        
        schedule$server("schedule")
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
      
      } else if (current == "myPlayer/" & !loadedServer$myPlayer) {
        
        myPlayer$server("myPlayer", auth = authOutput(), updated = updated)
        loadedServer$myPlayer <- TRUE
        
      } else if (current == "myBank" & !loadedServer$myBank) {
        
        myBank$server("myBank", auth = authOutput(), updated = updated)
        loadedServer$myBank <- TRUE
        
      } else if (current == "filework/export" & !loadedServer$export) {
        
        export$server("export", auth = authOutput(), updated = updated)
        loadedServer$export <- TRUE
        
      } else if (current == "filework/import" & !loadedServer$import) {
        
        import$server("import", auth = authOutput(), updated = updated())
        loadedServer$import <- TRUE
        
      } else if (current == "myPlayer/update" & !loadedServer$playerUpdate) {
        
        playerUpdate$server("update", auth = authOutput(), updated = updated, type = "update")
        loadedServer$playerUpdate <- TRUE
        
      } else if (current == "myPlayer/reroll" & !loadedServer$playerReroll) {
        
        playerUpdate$server("reroll", auth = authOutput(), updated = updated, type = "reroll")
        loadedServer$playerReroll <- TRUE
        
      } else if (current == "myPlayer/redistribute" & !loadedServer$playerRedist) {
        
        playerUpdate$server("redist", auth = authOutput(), updated = updated, type = "redistribution")
        loadedServer$playerRedist <- TRUE
        
      } else if (current == "myPlayer/regress" & !loadedServer$playerRegress) {
        
        playerUpdate$server("regress", auth = authOutput(), updated = updated, type = "regression")
        loadedServer$playerRegress <- TRUE
        
      } else if (current == "createPlayer" & !loadedServer$create) {
        
        createPlayer$server("create", auth = authOutput(), updated = updated)
        loadedServer$create <- TRUE
        
      } else if (current == "bank/deposit" & !loadedServer$bankDeposit) {
        
        bankDeposit$server("bankDeposit", auth = authOutput(), updated = updated)
        loadedServer$bankDeposit <- TRUE
        
      } else if (current == "bank/process" & !loadedServer$bankProcess) {
        
        bankProcess$server("bankProcess", auth = authOutput(), updated = updated)
        loadedServer$bankProcess <- TRUE
        
      } 
    }) |>
      shiny$bindEvent(session$clientData$url_hash)
  })
}
