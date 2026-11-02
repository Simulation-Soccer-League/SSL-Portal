box::use(
  bslib,
  lubridate[now],
  methods[is],
  shiny[
    a,
    actionButton,
    actionLink,
    bindEvent,
    div,
    icon,
    modalButton,
    modalDialog,
    moduleServer,
    NS,
    observe,
    p,
    passwordInput,
    reactive,
    renderText,
    renderUI,
    removeModal,
    selectInput,
    showModal,
    tagAppendAttributes,
    tagList,
    tags,
    textInput,
    uiOutput,
    verbatimTextOutput
  ],
  shiny.router[change_page, route_link],
  shinyFeedback[feedbackWarning, showToast],
  stringr[str_split],
)


box::use(
  app/logic/constant,
  app/logic/ui/tags[flexCol, flexRow, navMenu, navMenuItem],
  app/logic/db/login[
    customCheckCredentials, 
    getRefreshToken, 
    setRefreshToken,
    isBankerAccountant,
    isBoD,
    isBoDIntern,
    isDepartmentHead,
    isFileworker,
    isManager,
    isPT,
    isBountyGrader,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/player/playerChecks[checkApprovingPlayer, hasActivePlayer],
)

values <- seq_len(constant$currentSeason$season) |> 
  sort(decreasing = TRUE) 
names(values) <- paste("Season", values)

getNavItems <- function(ns, suffix) {
  tagList(
    flexRow(
      tagList(
        navMenu(
          div(a("Home", href = route_link("/")))
        ),
        navMenu(
          label = "Trackers",
          items = list(
            a("Search", href = route_link("search")),
            a("Organizations", href = route_link("tracker/organizations")),
            a("Draft Class", href = route_link("tracker/draftclass")),
            a("Position Tracker", href = route_link("tracker/position")),
            a("WSFC Regions", href = route_link("tracker/wsfc"))
          )
        ),
        navMenu(
          label = "Index",
          items = list(
            a("Index", href = route_link("index/")),
            navMenuItem(
              label = "Career",
              subItems = list(
                a("Index", href = route_link("index/career")),
                a("Records", href = route_link("index/records"))
              )
            ),
            a("Standings", href = route_link("index/standings")),
            a("Schedule", href = route_link("index/schedule")),
            navMenuItem(
              label = "Academy",
              subItems = list(
                a("Academy Index", href = route_link("index/academy")),
                a("Academy Standings & Schedule", href = route_link("index/academyStandings"))
              )
            )
          )
        ),
        uiOutput(ns(paste0("jobsNavigation", suffix))) |>
          withSpinnerCustom(height = 20)
      )
    ),
    flexRow(
      # Dropdown (full width)
      div(
        class = "seasonDropdown",
        
        selectInput(
          inputId = ns("selectedSeason"),
          label = NULL,
          choices = values,
          width = "100%"
        )
      )
    ),
    uiOutput(ns(paste0("yourPlayer", suffix))) |>
      withSpinnerCustom(height = 20)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    ## Function that loads js-cookies for auto-login
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script("// script.js
                    function getCookies(){
                      var res = Cookies.get();

                      Shiny.setInputValue('app-nav-token', res.token, {priority:'event'});
                    }

                  // script.js
                    Shiny.addCustomMessageHandler('cookie-set', function(msg){
                      Cookies.set(msg.name, msg.value);
                      getCookies();
                    })

                    Shiny.addCustomMessageHandler('cookie-remove', function(msg){
                      Cookies.remove(msg.name);
                      getCookies();
                    })

                  $(document).on('shiny:connected', function(ev){
                    getCookies();
                  });"),
    tags$head(
      tags$link(
        rel = "icon",
        type = "image/png",
        href = "favicon.ico"
      ),
      tags$title("SSL Portal")
    ),
    actionButton(
      inputId = "navToggle",
      label = tagList(
        div(
          icon("bars"),
          class = "nav-toggle-icon_closed"
        ),
        div(
          icon("xmark"),
          class = "nav-toggle-icon-open"
        )
      ),
      class = "nav-toggle",
      `aria-label` = "Toggle menu",
      `aria-expanded` = "false",
      onclick = "
        var isOpen = document.querySelector('.nav-container-narrow').classList.toggle('is-open');

        this.classList.toggle('is-open', isOpen);
        this.setAttribute('aria-expanded', isOpen);
        document.body.classList.toggle('nav-open', isOpen);
      "
    ),
    tags$nav(
      class = "nav-container-narrow",
      tagList(
        tags$img(src = "static/portalwhite.png", class = "logo-narrow", alt = "SSL Portal"),
        getNavItems(ns, "Mobile")
      )
    ),
    tags$nav(
      class = "ssl-navbar",
      tagList(
        tags$a(
          href = "https://forum.simulationsoccer.com",
          target = "_blank",
          tags$img(src = "static/portalwhite.png", alt = "SSL Portal"),
          class = "logo"
        ),
        div(
          class = "nav-container",
          getNavItems(ns, "Desktop")
        )
      )
    )
  )
}

#' @export
server <- function(id, auth, resAuth, updated, season) {
  moduleServer(id, function(input, output, session) {
    ### Output
    getJobsUi <- function(userGroup) {
      if (any(
        isDepartmentHead(userGroup),
        isFileworker(userGroup), 
        isBoD(userGroup), 
        isBoDIntern(userGroup),
        isBankerAccountant(userGroup),
        isManager(userGroup),
        isPT(userGroup),
        isBountyGrader(userGroup)
      )) {
        items <- list(
          if (
            any(
              isDepartmentHead(userGroup),
              isBankerAccountant(userGroup), 
              isPT(userGroup), 
              isBoD(userGroup),
              isBoDIntern(userGroup), 
              isManager(userGroup),
              isBountyGrader(userGroup)
              )
            ) {
            navMenuItem(
              label = "Bank",
              subItems = list(
                a("Bank Deposits", href = route_link("bank/deposit")),
                if (
                  any(
                    isBankerAccountant(userGroup), 
                    isBoD(userGroup),
                    isBoDIntern(userGroup)
                  )
                ) {
                  a("Process Transactions", href = route_link("bank/process"))
                }
              )
            )
          },
          if (any(isBoD(userGroup), isBoDIntern(userGroup))) {
            navMenuItem(
              label = "Board of Directors",
              subItems = list(
                a("Approve Players", href = route_link("bod/approve")),
                a("Assign Managers", href = route_link("bod/manager")),
                a("Edit Player", href = route_link("bod/edit"))
              )
            )
          },
          if (any(isFileworker(userGroup), isBoD(userGroup), isBoDIntern(userGroup))) {
            navMenuItem(
              label = "File Work",
              subItems = list(
                a("Build Exports", href = route_link("filework/export")),
                a("Index Imports", href = route_link("filework/import")),
                a("Edit Schedule", href = route_link("filework/scheduleEdit"))
              )
            )
          },
          if (any(isManager(userGroup), isBoD(userGroup))) {
            navMenuItem(
              label = "Manager",
              subItems = list(
                a("Roster Overview", href = route_link("manager/overview"))
              )
            )
          },
          if (any(isPT(userGroup), isBoD(userGroup), isBoDIntern(userGroup))) {
            navMenuItem(
              label = "PT",
              subItems = list(
                a("PT Deposit", href = route_link("pt/deposit"))
              )
            )
          }
        )
        navMenu(
          label = "Jobs",
          items = items
        )
      }
    }

    output$jobsNavigationDesktop <- renderUI({
      getJobsUi(auth()$usergroup)
    }) |>
      bindEvent(auth())

    output$jobsNavigationMobile <- renderUI({
      getJobsUi(auth()$usergroup)
    }) |>
      bindEvent(auth())

    getPlayerUi <- function(userGroup) {
      if (userGroup |> is.null()) {
        navMenu(
          actionLink("Login", icon = icon("user"), inputId = session$ns("login"), class = "nav-login")
        )
      } else {
        playerItem <- if (checkApprovingPlayer(auth()$uid)) {
          div(class = "nav-menu-note", "Awaiting Approval")
        } else if (!hasActivePlayer(auth()$uid)) {
          actionLink("Create a Player", icon = icon("square-plus"), inputId = session$ns("create"))
        } else {
          navMenuItem(
            label = "Player",
            subItems = list(
              a("My Player", href = route_link("myPlayer/")),
              a("Bank/Store", href = route_link("myBank"))
            )
          )
        }

        navMenu(
          label = auth()$username,
          items = list(
            playerItem,
            actionLink("Logout", icon = icon("door-open"), inputId = session$ns("logout"))
          )
        )
      }
    }

    output$yourPlayerDesktop <- renderUI({
      getPlayerUi(auth()$usergroup)
    }) |>
      bindEvent(auth())

    output$yourPlayerMobile <- renderUI({
      getPlayerUi(auth()$usergroup)
    }) |>
      bindEvent(auth())

    ### Observers
    # Changes season
    observe({
      season(input$selectedSeason)
    }) |> 
      bindEvent(input$selectedSeason)
    
    
    # Checks saved cookie for automatic login
    observe({
      refreshtoken <- getRefreshToken(input$token)
      
      if (refreshtoken |> nrow() > 0) {
        if ((now() |> as.numeric()) < refreshtoken$expires_at) {
          resAuth$uid <- refreshtoken$uid
          resAuth$username <- refreshtoken$username
          resAuth$usergroup <-
            paste(refreshtoken$usergroup, refreshtoken$additionalgroups, sep = ",") |>
            str_split(pattern = ",", simplify = TRUE) |>
            as.numeric() |>
            as.list()
          resAuth$suspended <- refreshtoken$suspendposting == 1
          setRefreshToken(uid = refreshtoken$uid, token = refreshtoken$token)
        }
      }
    }) |>
      bindEvent(input$token, ignoreNULL = TRUE, once = TRUE)

    observe({
      showModal(
        modalDialog(
          footer = NULL,
          easyClose = TRUE,
          div(
            class = "login-modal",
            onkeydown = sprintf(
              "if (event.key === 'Enter' && event.target.tagName === 'INPUT') {
                event.preventDefault();
                // Leaving the field sends its value to Shiny before the login click does
                event.target.blur();
                document.getElementById('%s').click();
              }",
              session$ns("loggingIn")
            ),
            tags$script(
              sprintf(
                "$('#shiny-modal').one('shown.bs.modal', function() { $('#%s').focus(); });",
                session$ns("user")
              )
            ),
            tags$button(
              class = "login-close",
              type = "button",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              `aria-label` = "Close",
              icon("xmark")
            ),
            div(
              class = "login-head",
              tags$img(src = "static/portalwhite.png", alt = "SSL Portal"),
              tags$h4("Welcome back"),
              p("Log in with your forum account.")
            ),
            div(
              class = "login-field",
              icon("user"),
              textInput(session$ns("user"), label = "Username") |>
                tagAppendAttributes(autocomplete = "username", .cssSelector = "input")
            ),
            div(
              class = "login-field",
              icon("lock"),
              passwordInput(session$ns("password"), label = "Password") |>
                tagAppendAttributes(autocomplete = "current-password", .cssSelector = "input"),
              tags$button(
                class = "login-password-toggle",
                type = "button",
                `aria-label` = "Show or hide password",
                onclick = "
                  var input = this.parentElement.querySelector('input');
                  var isHidden = input.type === 'password';
                  var eye = this.querySelector('i');

                  input.type = isHidden ? 'text' : 'password';
                  eye.classList.toggle('fa-eye', !isHidden);
                  eye.classList.toggle('fa-eye-slash', isHidden);
                ",
                icon("eye")
              )
            ),
            actionButton(session$ns("loggingIn"), "Login", class = "login-submit"),
            div(
              class = "login-links",
              tags$a(
                "Register a new user!",
                href = "https://forum.simulationsoccer.com/member.php?action=register",
                target = "_blank"
              ),
              tags$a(
                "Forgot password?",
                href = "https://forum.simulationsoccer.com/member.php?action=lostpw",
                target = "_blank"
              )
            )
          )
        )
      )
    }) |>
      bindEvent(input$login)

    observe({
      res <- customCheckCredentials(user = input$user, password = input$password)
      if (res$result) {
        removeModal()
        resAuth$uid <- res$userInfo$uid
        resAuth$username <- res$userInfo$username
        resAuth$usergroup <- res$userInfo$usergroup
        resAuth$suspended <- res$userInfo$suspended
        
        updated(updated() + 1)
      } else {
        feedbackWarning("password", show = TRUE, text = "Password is incorrect.")
      }
    }) |>
      bindEvent(input$loggingIn)
    
    ## Changing page to create
    observe({
      change_page("createPlayer")
    }) |> 
      bindEvent(input$create)
    
    ## Logging out
    observe({
      resAuth$uid <- NULL
      resAuth$username <- NULL
      resAuth$usergroup <- NULL
      resAuth$suspended <- 0
      
      change_page("")
      
      updated(updated() + 1)
      
      msg <- list(name = "token")
      session$sendCustomMessage("cookie-remove", msg)
    }) |> 
      bindEvent(input$logout)
  })
}
