box::use(
  future[multisession, plan],
  shiny,
  shiny.router[route, router_server, router_ui],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
  stringr[str_detect, str_remove],
)

box::use(
  app/view/navigationBar,
  app/view/tracker/player,
  app/view/tracker/playerSearch,
  app/view/welcome,
)

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
      route("tracker/player", player$ui(ns("player"))),
      route("search", playerSearch$ui(ns("search")))
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
      usergroup = NULL
    )
    
    # Adds all authentication list to a reactive object
    authOutput <- shiny$reactive({
      shiny$reactiveValuesToList(resAuth)
    })
    
    navigationBar$server("nav", auth = authOutput, resAuth = resAuth)
    
    welcome$server("welcome", usergroup = authOutput()$usergroup)
    
    playerSearch$server("search")
    
    ## In order to load pages as they are clicked ONCE this is needed
    loadedServer <-
      shiny$reactiveValues(
        create = FALSE, player = FALSE, index = FALSE,
        academy = FALSE, uploadGame = FALSE,
        bankOverview = FALSE, welcome = FALSE, records = FALSE,
        playerPages = FALSE, contractProcess = FALSE,
        tradeProcess = FALSE, playerEdit = FALSE, submitPT = FALSE,
        bankDeposit = FALSE, bankProcess = FALSE,
        standings = FALSE, schedule = FALSE, managerTeam = FALSE,
        assignManager = FALSE, bodoverview = FALSE, exportBuild = FALSE,
        organization = FALSE, draftclass = FALSE, nationTracker = FALSE,
        positionTracker = FALSE
      )
    
    ## Observer that checks the current page and loads the server for the page ONCE
    shiny$observe({
      current <- str_remove(session$clientData$url_hash,
                            pattern = "#!/")
      
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
         player$server("player")
         loadedServer$player <- TRUE
      }
    }) |>
      shiny$bindEvent(session$clientData$url_hash)
    
  })
}
