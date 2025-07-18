box::use(
  lubridate[now],
  methods[is],
  shiny[moduleServer, NS, tagList, tags, 
        icon, div, uiOutput, renderUI, 
        observe, bindEvent, a, actionButton, 
        p, showModal, removeModal, modalDialog, 
        modalButton, textInput, passwordInput, 
        verbatimTextOutput, renderText, reactive,
        actionLink],
  shiny.router[change_page, route_link],
  shinyFeedback[feedbackWarning],
  stringr[str_split],
)


box::use(
  app/logic/ui/tags[flexCol, flexRow, navMenu, navMenuItem],
  app/logic/db/login[
    customCheckCredentials, 
    getRefreshToken, 
    setRefreshToken,
    isBankerAccountant,
    isBoD,
    isBoDIntern,
    isFileworker,
    isManager,
    isPT,
  ],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/player/playerChecks[checkApprovingPlayer, hasActivePlayer],
)

getNavItems <- function(ns, suffix) {
  tagList(
    flexRow(
      tagList(
        navMenu(
          label = "Trackers",
          items = list(
            a("Search", href = route_link("search")),
            a("Organizations", href = route_link("tracker/organization")),
            a("Draft Class", href = route_link("tracker/draftclass"))
          )
        ),
        navMenu(
          label = "Index",
          items = list(
            a("Index", href = route_link("index/")),
            a("Records", href = route_link("index/records")),
            a("Standings", href = route_link("index/standings")),
            a("Schedule", href = route_link("index/schedule")),
            a("Academy", href = route_link("index/academy"))
          )
        ),
        uiOutput(ns(paste0("jobsNavigation", suffix))) |>
          withSpinnerCustom(height = 20),
        navMenu(
          div(a("Intro", href = route_link("/")))
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
                      console.log(res.token);

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
    tags$nav(
      class = "nav-container-narrow",
      tagList(
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
          onclick = "
            var mobileNav = document.querySelector('.nav-container-narrow');
            var openMaxWidth = '80%';

            if (mobileNav) {
              var isOpen = getComputedStyle(mobileNav).maxWidth === openMaxWidth;
              mobileNav.style.maxWidth = isOpen ? '0px' : openMaxWidth;
              this.style.left = isOpen ? '0px' : `calc(${openMaxWidth} - 40px)`;
              this.querySelector('.nav-toggle-icon_closed').style.display = isOpen ? 'block' : 'none';
              this.querySelector('.nav-toggle-icon-open').style.display = isOpen ? 'none' : 'block';
            }
        "
        ),
        getNavItems(ns, "Mobile")
      )
    ),
    tags$nav(
      class = "ssl-navbar",
      tagList(
        tags$a(
          href = "https://forum.simulationsoccer.com",
          target = "_blank",
          tags$img(src = "static/portalwhite.png", height = "70"),
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
server <- function(id, auth, resAuth, updated) {
  moduleServer(id, function(input, output, session) {
    ### Output
    getJobsUi <- function(userGroup) {
      if (any(
        isFileworker(userGroup), 
        isBoD(userGroup), 
        isBoDIntern(userGroup),
        isBankerAccountant(userGroup),
        isManager(userGroup),
        isPT(userGroup)
      )) {
        items <- list(
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
          if (any(isBankerAccountant(userGroup), isPT(userGroup), isBoD(userGroup),
                  isBoDIntern(userGroup), isManager(userGroup))) {
            navMenuItem(
              label = "Bank",
              subItems = list(
                a("Bank Deposits", href = route_link("bank/deposit")),
                if (any(isBankerAccountant(userGroup), isBoD(userGroup),
                        isBoDIntern(userGroup))) {
                  a("Process Transactions", href = route_link("bank/process"))
                }
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
          actionLink("Login", icon = icon("user"), inputId = session$ns("login"))
        )
      } else {
        if(checkApprovingPlayer(auth()$uid)){
          playerMenu <- "Awating Approval"
        } else if(!hasActivePlayer(auth()$uid)){
          playerMenu <- 
            navMenu(
              actionLink("Create a Player", icon = icon("square-plus"), inputId = session$ns("create"))
            )
        } else {
          playerMenu <- 
            navMenu(
              label = "Player",
              items = list(
                a("My Player", href = route_link("myPlayer/")),
                a("Bank/Store", href = route_link("myBank"))
              ),
              showItems = TRUE
            )
        }
        
        flexRow(
          tagList(
            playerMenu,
            navMenu(
              actionLink("Logout", icon = icon("door-open"), inputId = session$ns("logout"))
            )
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
          textInput(session$ns("user"), label = "Username:"),
          passwordInput(session$ns("password"), label = "Password:"),
          footer = tagList(
            modalButton("Cancel"), actionButton(session$ns("loggingIn"), "Login"),
            tags$div(
              tags$a(
                "Register a new user!",
                href = "https://forum.simulationsoccer.com/member.php?action=register",
                target = "_blank", 
                style = "float: left;"
              ),
              tags$a(
                "Forgot password?",
                href = "https://forum.simulationsoccer.com/member.php?action=lostpw",
                target = "_blank", 
                style = "float:right;"
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
