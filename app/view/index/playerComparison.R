box::use(
  bslib,
  shiny,
  plotly,
  stats[setNames],
  dplyr[filter],
  reactable[
    reactable,
    reactableOutput,
    renderReactable,
    colDef
  ]
)

box::use(
  app / logic / constant,

  app / logic / get/ getPlayer[
    getPlayer,
    getPlayerNames,
    getOrganizations
  ],

  app / logic / get / getIndex[getLeagueIndex],

  app / logic / ui / reactableHelper[
    linkOrganization,
    comparisonBar
  ],

  app / logic / ui / spinner[withSpinnerCustom]
)

# Pill between the two player selectors
vsBadge <- function() {

  shiny$div(
    "VS",
    class = "player-compare-vs",
    style = paste0(
      "font-weight:700;
      font-size:13px;
      letter-spacing:0.5px;
      color:", constant$sslGold, ";
      border:1px solid ", constant$sslGold, "55;
      background:", constant$sslGold, "1A;
      border-radius:999px;
      padding:6px 14px;
      white-space:nowrap;"
    )
  )

}

#' @export
ui <- function(id) {

  ns <- shiny$NS(id)

  shiny$tagList(

    bslib$card(

      bslib$card_header(
        shiny$h2("Player Comparison", style = "margin:0;"),
        shiny$p(
          "Pick any two players to compare their career stats and attributes side by side.",
          class = "text-muted",
          style = "margin:4px 0 0 0; font-size:14px;"
        )
      ),

      bslib$card_body(

        shiny$div(
          class = "player-compare-select-row",

          shiny$div(
            class = "player-compare-select",
            shiny$selectizeInput(
              ns("player1"),
              "Player 1",
              choices = NULL,
              multiple = FALSE,
              width = "100%",
              options = list(
                placeholder = "Start typing a player name..."
              )
            ),
            shiny$uiOutput(ns("player1Filters"))
          ),

          vsBadge(),

          shiny$div(
            class = "player-compare-select",
            shiny$selectizeInput(
              ns("player2"),
              "Player 2",
              choices = NULL,
              multiple = FALSE,
              width = "100%",
              options = list(
                placeholder = "Start typing a player name..."
              )
            ),
            shiny$uiOutput(ns("player2Filters"))
          )

        ),

        shiny$uiOutput(ns("comparison")) |>
          withSpinnerCustom(height = 40)

      )

    ),

    # Attack/Creative/Defense/Physical/Goalkeeping - hidden until both players are picked
    shiny$uiOutput(ns("comparisonDetails")) |>
      withSpinnerCustom(height = 40)

  )

}

#' @export
server <- function(id) {

  shiny$moduleServer(id, function(input, output, session) {

    #### PLAYER LIST ####

    players <- shiny$reactive({

      getPlayerNames(TRUE)

    })

    #### LOAD SELECTS ####

    shiny$observe({

      data <- players()

      choices <- stats::setNames(
        data$pid,
        paste0(
          data$name,
          " (",
          data$team,
          ")"
        )
      )

      shiny$updateSelectizeInput(
        session,
        "player1",
        choices = choices,
        selected = character(0),
        server = TRUE
      )

      shiny$updateSelectizeInput(
        session,
        "player2",
        choices = choices,
        selected = character(0),
        server = TRUE
      )

    })

    #### PLAYER DATA ####

    player1 <- shiny$reactive({

      shiny$req(input$player1)

      getPlayer(
        as.numeric(input$player1)
      )

    })

    player2 <- shiny$reactive({

      shiny$req(input$player2)

      getPlayer(
        as.numeric(input$player2)
      )

    })

    organizations <- shiny$reactive({

    getOrganizations()

    })    

    player1Color <- shiny$reactive({

    shiny$req(player1())

    organizations() |>
        dplyr::filter(
        name == player1()$team
        ) |>
        dplyr::pull(primaryColor) |>
        dplyr::first()

    })

    player2Color <- shiny$reactive({

    shiny$req(player2())

    organizations() |>
        dplyr::filter(
        name == player2()$team
        ) |>
        dplyr::pull(primaryColor) |>
        dplyr::first()

    })

    player2SecondaryColor <- shiny$reactive({

    shiny$req(player2())

    organizations() |>
        dplyr::filter(
        name == player2()$team
        ) |>
        dplyr::pull(secondaryColor) |>
        dplyr::first()

    })

    # Straight-line RGB distance between two "#RRGGBB" colors (max ~441.7)
    colorDistance <- function(hex1, hex2) {

        toRgb <- function(hex) {
            hex <- sub("^#", "", hex)
            c(
                strtoi(substr(hex, 1, 2), 16L),
                strtoi(substr(hex, 3, 4), 16L),
                strtoi(substr(hex, 5, 6), 16L)
            )
        }

        sqrt(sum((toRgb(hex1) - toRgb(hex2))^2))

    }

    # Falls back to player 2's secondary color if the primaries are too close
    player2RadarColor <- shiny$reactive({

    shiny$req(player1Color(), player2Color())

    tooClose <- colorDistance(player1Color(), player2Color()) < 70

    if (!tooClose) {
        return(player2Color())
    }

    secondary <- player2SecondaryColor()

    if (is.null(secondary) || is.na(secondary) || !nzchar(secondary)) {
        return(player2Color())
    }

    secondary

    })

    #### CAREER DATA (season aware) ####

    # Mirrors the query on the player's own "Career Statistics" table
    getSeasonHistory <- function(playerName) {

      dplyr::bind_rows(
        getLeagueIndex(
          outfield = TRUE, season = "ALL", league = "ALL", name = playerName, career = TRUE
        ),
        getLeagueIndex(
          outfield = FALSE, season = "ALL", league = "ALL", name = playerName, career = TRUE
        )
      )

    }

    getAvailableSeasons <- function(playerName) {

      rows <- getSeasonHistory(playerName)

      if (nrow(rows) == 0 || !("max_season" %in% names(rows))) {
        return(numeric(0))
      }

      rows$max_season |> unique() |> sort(decreasing = TRUE)

    }

    # setNames() errors on a length-0 seasons vector, hence the early return
    buildSeasonChoices <- function(seasons) {

      if (length(seasons) == 0) {
        return(c("Career" = "ALL"))
      }

      c("Career" = "ALL", stats::setNames(as.character(seasons), paste0("Season ", seasons)))

    }

    resolveSeasonParam <- function(seasonFilter) {
      if (is.null(seasonFilter) || seasonFilter == "ALL") "ALL" else as.numeric(seasonFilter)
    }

    # Season filter runs SQL-side, so this is always one aggregated row
    getPlayerCareerRow <- function(playerName, seasonFilter) {

      seasonParam <- resolveSeasonParam(seasonFilter)

      outfieldRow <- getLeagueIndex(
        outfield = TRUE, season = seasonParam, league = "ALL", name = playerName
      )

      if (nrow(outfieldRow) > 0) {
        return(outfieldRow)
      }

      # Pure goalkeepers have no outfield rows at all
      getLeagueIndex(outfield = FALSE, season = seasonParam, league = "ALL", name = playerName)

    }

    getPlayerKeeperRow <- function(playerName, seasonFilter) {

      seasonParam <- resolveSeasonParam(seasonFilter)

      getLeagueIndex(outfield = FALSE, season = seasonParam, league = "ALL", name = playerName)

    }

    output$player1Filters <- shiny$renderUI({

      shiny$req(player1())

      seasons <- getAvailableSeasons(player1()$name)

      shiny$div(
        style = "margin-top:8px; font-size:13px;",
        shiny$selectInput(
          session$ns("player1Season"),
          "Season",
          choices = buildSeasonChoices(seasons),
          selected = "ALL",
          width = "100%"
        )
      )

    })

    output$player2Filters <- shiny$renderUI({

      shiny$req(player2())

      seasons <- getAvailableSeasons(player2()$name)

      shiny$div(
        style = "margin-top:8px; font-size:13px;",
        shiny$selectInput(
          session$ns("player2Season"),
          "Season",
          choices = buildSeasonChoices(seasons),
          selected = "ALL",
          width = "100%"
        )
      )

    })

    careerPlayer1 <- shiny$reactive({

        shiny$req(player1())

        season <- if (is.null(input$player1Season)) "ALL" else input$player1Season

        getPlayerCareerRow(player1()$name, season)

    })

    careerPlayer2 <- shiny$reactive({

        shiny$req(player2())

        season <- if (is.null(input$player2Season)) "ALL" else input$player2Season

        getPlayerCareerRow(player2()$name, season)

    })

    careerKeeperPlayer1 <- shiny$reactive({

        shiny$req(player1())

        season <- if (is.null(input$player1Season)) "ALL" else input$player1Season

        getPlayerKeeperRow(player1()$name, season)

    })

    careerKeeperPlayer2 <- shiny$reactive({

        shiny$req(player2())

        season <- if (is.null(input$player2Season)) "ALL" else input$player2Season

        getPlayerKeeperRow(player2()$name, season)

    })

    safeStat <- function(df, col) {

        if (
            is.null(df) ||
            nrow(df) == 0 ||
            !(col %in% names(df))
        ) {
            return(0)
        }

        value <- df[[col]]

        if (length(value) == 0 || is.na(value)) {
            return(0)
        }

        value

    }

    getStatValue <- function(df, stat) {

    if (stat == "saves") {

        return(
        safeStat(df, "saves parried") +
        safeStat(df, "saves held") +
        safeStat(df, "saves tipped")
        )

    }

    safeStat(df, stat)

    }    

    statLabels <- c(

    # Attack
    "xg" = "xG",
    "pen adj xG" = "Pen Adj xG",
    "xg overperformance" = "xG Overperformance",
    "shots on target" = "Shots On Target",
    "shot accuracy%" = "Shot Accuracy %",
    "goals outside box" = "Goals Outside Box",
    "fk shots" = "FK Shots",

    # Creative
    "xa" = "xA",
    "pass%" = "Pass %",
    "open play key passes" = "Open Play Key Passes",
    "successful passes" = "Successful Passes",
    "attempted passes" = "Attempted Passes",
    "key passes" = "Key Passes",
    "chances created" = "Chances Created",
    "progressive passes" = "Progressive Passes",
    "successful crosses" = "Successful Crosses",
    "attempted crosses" = "Attempted Crosses",
    "cross%" = "Cross %",
    "successful open play crosses" = "Successful Open Play Crosses",
    "attempted open play crosses" = "Attempted Open Play Crosses",
    "open play crosses%" = "Open Play Crosses %",

    # Defense
    "tackles won" = "Tackles Won",
    "attempted tackles" = "Attempted Tackles",
    "tackle%" = "Tackle %",
    "key tackles" = "Key Tackles",
    "mistakes leading to goals" = "Mistakes Leading To Goals",
    "shots blocked" = "Shots Blocked",
    "successful presses" = "Successful Presses",
    "attempted presses" = "Attempted Presses",
    "press%" = "Press %",

    # Physical
    "minutes played" = "Minutes Played",
    "average rating" = "Average Rating",
    "distance run (km)" = "Distance Run (KM)",
    "player of the match" = "Player Of The Match",
    "successful headers" = "Successful Headers",
    "attempted headers" = "Attempted Headers",
    "header%" = "Header %",
    "key headers" = "Key Headers",
    "fouls against" = "Fouls Against",
    
    #Goalkeeping
    "clean sheets" = "Clean Sheets",
    "conceded" = "Conceded",
    "saves" = "Saves",
    "saves parried" = "Saves Parried",
    "saves held" = "Saves Held",
    "saves tipped" = "Saves Tipped",
    "save%" = "Save %",
    "penalties faced" = "Penalties Faced",
    "penalties saved" = "Penalties Saved",
    "xsave%" = "xSave %",
    "xg prevented" = "xG Prevented"            
    )

    # Stats where lower is better - everything else defaults to higher = better
    lowerIsBetterStats <- c(
        "conceded",
        "mistakes leading to goals",
        "fouls",
        "yellow cards",
        "red cards",
        "offsides"
    )

    getWinner <- function(leftVal, rightVal, stat) {

        if (is.na(leftVal) || is.na(rightVal) || leftVal == rightVal) {
            return("tie")
        }

        leftIsHigher <- leftVal > rightVal

        if (stat %in% lowerIsBetterStats) {
            leftIsHigher <- !leftIsHigher
        }

        if (leftIsHigher) "left" else "right"

    }

    buildStatData <- function(cp1, cp2, stats) {

        leftVals <- unname(
            sapply(
                stats,
                function(x) getStatValue(cp1, x)
            )
        )

        rightVals <- unname(
            sapply(
                stats,
                function(x) getStatValue(cp2, x)
            )
        )

        df <- data.frame(

            Left = leftVals,

            Stat = sapply(
                stats,
                function(x) {

                    if (x %in% names(statLabels)) {
                        statLabels[[x]]
                    } else {
                        tools::toTitleCase(x)
                    }

                }
            ),

            Right = rightVals,

            Winner = mapply(
                getWinner,
                leftVals,
                rightVals,
                stats
            ),

            check.names = FALSE,
            stringsAsFactors = FALSE

        )

        rownames(df) <- NULL

        df

    }

    attackStats <- c(
    "goals",
    "xg",
    "pen adj xG",
    "xg overperformance",
    "shots",
    "shots on target",
    "shot accuracy%",
    "goals outside box",
    "fk shots",
    "penalties taken",
    "penalties scored",
    "offsides"
    )

    creativeStats <- c(
    "assists",
    "xa",
    "successful passes",
    "attempted passes",
    "pass%",
    "key passes",
    "open play key passes",
    "chances created",
    "progressive passes",
    "successful crosses",
    "attempted crosses",
    "cross%",
    "successful open play crosses",
    "attempted open play crosses",
    "open play crosses%"
    )

    defenseStats <- c(
    "tackles won",
    "attempted tackles",
    "tackle%",
    "key tackles",
    "interceptions",
    "clearances",
    "blocks",
    "shots blocked",
    "successful presses",
    "attempted presses",
    "press%",
    "mistakes leading to goals"
    )

    physicalStats <- c(
    "apps",
    "minutes played",
    "distance run (km)",
    "average rating",
    "player of the match",
    "dribbles",
    "successful headers",
    "attempted headers",
    "header%",
    "key headers",
    "fouls",
    "fouls against",
    "yellow cards",
    "red cards"
    )

    goalkeepingStats <- c(
    "clean sheets",
    "conceded",
    "saves",
    "saves parried",
    "saves held",
    "saves tipped",
    "save%",
    "penalties faced",
    "penalties saved",
    "xsave%",
    "xg prevented"
    )    
         

    showGoalkeeping <- shiny$reactive({

        player1()$position == "GK" ||
        player2()$position == "GK"

    })

    showAttack <- shiny$reactive({

        !(
            player1()$position == "GK" &&
            player2()$position == "GK"
        )

    })    
   

    #### COMPARISON TABLES ####

    buildComparisonTable <- function(data, leftColor, rightColor) {

    reactable(

        data,

        pagination = FALSE,
        searchable = FALSE,
        sortable = FALSE,
        bordered = FALSE,
        highlight = FALSE,

        defaultColDef = colDef(
            vAlign = "center"
        ),

        columns = list(

        Winner = colDef(show = FALSE),

        Left = colDef(

            name = player1()$name,

            headerStyle = list(
                color = leftColor,
                textAlign = "center",
                fontWeight = "bold"
            ),
            minWidth = 150,

            cell = function(value, index) {

            leftVal <- suppressWarnings(
                as.numeric(data$Left[index])
            )

            rightVal <- suppressWarnings(
                as.numeric(data$Right[index])
            )

            if (
                is.na(leftVal) ||
                is.na(rightVal)
            ) {
                return("—")
            }

            total <- leftVal + rightVal

            if (total <= 0) {
                total <- 1
            }

            comparisonBar(
                leftVal,
                total,
                side = "right",
                highlight = data$Winner[index] == "left"
            )

            }

        ),

        Stat = colDef(
            name = "",
            align = "center",
            minWidth = 190,
            style = list(
                textAlign = "center",
                fontWeight = "600",
                opacity = "0.85"
            )
        ),

        Right = colDef(

            name = player2()$name,

            headerStyle = list(
                color = rightColor,
                textAlign = "center",
                fontWeight = "bold"
            ),
            minWidth = 150,

            cell = function(value, index) {

            leftVal <- suppressWarnings(
                as.numeric(data$Left[index])
            )

            rightVal <- suppressWarnings(
                as.numeric(data$Right[index])
            )

            if (
                is.na(leftVal) ||
                is.na(rightVal)
            ) {
                return("—")
            }

            total <- leftVal + rightVal

            if (total <= 0) {
                total <- 1
            }

            comparisonBar(
                rightVal,
                total,
                side = "left",
                highlight = data$Winner[index] == "right"
            )

            }

        )

        )

    )

    }

    attackData <- shiny$reactive({

    buildStatData(
        careerPlayer1(),
        careerPlayer2(),
        attackStats
    )

    })

    creativeData <- shiny$reactive({

    buildStatData(
        careerPlayer1(),
        careerPlayer2(),
        creativeStats
    )

    })

    defenseData <- shiny$reactive({

    buildStatData(
        careerPlayer1(),
        careerPlayer2(),
        defenseStats
    )

    })

    physicalData <- shiny$reactive({

    buildStatData(
        careerPlayer1(),
        careerPlayer2(),
        physicalStats
    )

    })

    goalkeepingData <- shiny$reactive({

    buildStatData(
        careerKeeperPlayer1(),
        careerKeeperPlayer2(),
        goalkeepingStats
    )

    })

    # Nothing (not even the card shells) shows until both players are picked
    output$comparisonDetails <- shiny$renderUI({

    if (
        is.null(input$player1) || input$player1 == "" ||
        is.null(input$player2) || input$player2 == ""
    ) {
        return(NULL)
    }

    shiny$req(player1(), player2(), careerPlayer1(), careerPlayer2())

    attributesBody <- if (samePositionType()) {

        shiny$div(
            style = "display:flex; justify-content:center; width:100%;",
            plotly$plotlyOutput(
                session$ns("comparisonRadar"),
                height = "380px",
                width = "380px"
            )
        )

    } else {

        shiny$div(
            style = "display:flex; justify-content:center; flex-wrap:wrap; gap:16px; width:100%;",
            plotly$plotlyOutput(
                session$ns("comparisonRadarPlayer1"),
                height = "340px",
                width = "340px"
            ),
            plotly$plotlyOutput(
                session$ns("comparisonRadarPlayer2"),
                height = "340px",
                width = "340px"
            )
        )

    }

    shiny$div(
        style = "
            width:100%;
            max-width:1200px;
            margin:auto;
        ",

        bslib$card(

            class = "mb-4",

            bslib$card_header(
                shiny$h3(
                    "Attributes",
                    style = "margin:0;"
                )
            ),

            attributesBody

        ),

        if (showAttack()) {

            bslib$card(

                class = "mb-4",

                bslib$card_header(
                    shiny$h3(
                        "Attack",
                        style = "margin:0;"
                    )
                ),

                reactableOutput(
                    session$ns("attackTable")
                )

            )

        },

        bslib$card(

            class = "mb-4",

            bslib$card_header(
                shiny$h3(
                    "Creative",
                    style = "margin:0;"
                )
            ),

            reactableOutput(
            session$ns("creativeTable")
            )

        ),

        bslib$card(

            class = "mb-4",

            bslib$card_header(
                shiny$h3(
                    "Defense",
                    style = "margin:0;"
                )
            ),

            reactableOutput(
            session$ns("defenseTable")
            )

        ),

        bslib$card(

            class = "mb-4",

            bslib$card_header(
                shiny$h3(
                    "Physical",
                    style = "margin:0;"
                )
            ),

            reactableOutput(
            session$ns("physicalTable")
            )

        ),

        if (showGoalkeeping()) {

            bslib$card(

            class = "mb-4",

            bslib$card_header(
                shiny$h3(
                    "Goalkeeping",
                    style = "margin:0;"
                )
            ),

                reactableOutput(
                    session$ns("goalkeepingTable")
                )

            )

        }

    )

    })

    output$attackTable <- renderReactable({
    buildComparisonTable(attackData(), player1Color(), player2Color())
    })

    output$creativeTable <- renderReactable({
    buildComparisonTable(creativeData(), player1Color(), player2Color())
    })

    output$defenseTable <- renderReactable({
    buildComparisonTable(defenseData(), player1Color(), player2Color())
    })

    output$physicalTable <- renderReactable({
    buildComparisonTable(physicalData(), player1Color(), player2Color())
    })

    output$goalkeepingTable <- renderReactable({
    buildComparisonTable(goalkeepingData(), player1Color(), player2Color())
    })


    #### ATTRIBUTE RADAR ####

    # Each vertex is a composite (mean) of a handful of raw FM attributes
    outfieldRadarGroups <- list(
        "Speed" = c("acceleration", "pace", "agility"),
        "Attacking" = c("finishing", "long shots", "off the ball", "composure", "penalty taking"),
        "Defending" = c("tackling", "marking", "positioning", "anticipation", "bravery"),
        "Aerial" = c("jumping reach", "heading", "strength"),
        "Creativity" = c("vision", "passing", "flair", "crossing", "long throws"),
        "Technical" = c("technique", "first touch", "dribbling", "corners", "free kick")
    )

    # "Command of Area" is wrapped so plotly doesn't clip it at that angle
    keeperRadarGroups <- list(
        "Reflexes" = c("reflexes", "agility"),
        "Aerial Ability" = c("aerial reach", "jumping reach"),
        "Command<br>of Area" = c("command of area", "communication", "bravery"),
        "Distribution" = c("kicking", "throwing", "passing", "vision"),
        "Handling" = c("handling", "composure"),
        "One on Ones" = c("one on ones", "anticipation", "decisions")
    )

    getRadarCategoryValue <- function(playerRow, attrs) {
        mean(sapply(attrs, function(a) safeStat(playerRow, a)), na.rm = TRUE)
    }

    # Concentric grid rings - inner fainter, outer more visible
    radarLevels <- c(4, 8, 12, 16, 20)
    radarRingOpacities <- seq(0.08, 0.35, length.out = length(radarLevels))

    # Matches the portal's own body font stack (see app/styles/main.scss)
    radarFontFamily <- "'Gotham SSm A', 'Gotham SSm B', Helvetica, sans-serif"

    # Keeper category set only when BOTH players are keepers (mirrors showGoalkeeping())
    buildComparisonRadar <- function(player1Row, player2Row, color1, color2, name1, name2) {

        groups <- if (player1Row$pos_gk == 20 && player2Row$pos_gk == 20) {
            keeperRadarGroups
        } else {
            outfieldRadarGroups
        }

        categories <- names(groups)

        values1 <- sapply(groups, function(attrs) getRadarCategoryValue(player1Row, attrs))
        values2 <- sapply(groups, function(attrs) getRadarCategoryValue(player2Row, attrs))

        # Repeat the first vertex at the end so the polygon closes cleanly.
        theta <- c(categories, categories[1])
        r1 <- c(values1, values1[1])
        r2 <- c(values2, values2[1])

        radar <- plotly$plot_ly()

        for (i in seq_along(radarLevels)) {
            radar <- radar |>
                plotly$add_trace(
                    type = "scatterpolar",
                    r = rep(radarLevels[i], length(theta)),
                    theta = theta,
                    mode = "lines",
                    line = list(
                        color = sprintf("rgba(255,255,255,%.2f)", radarRingOpacities[i]),
                        width = 1
                    ),
                    fill = "none",
                    hoverinfo = "skip",
                    showlegend = FALSE
                )
        }

        radar |>
            plotly$add_trace(
                type = "scatterpolar",
                r = r1,
                theta = theta,
                mode = "lines+markers",
                name = name1,
                line = list(color = color1, width = 2),
                marker = list(color = color1, size = 6),
                fill = "toself",
                fillcolor = paste0(color1, "26"),
                hoverinfo = "text",
                hovertext = sprintf("%s - %s: %.1f", name1, theta, r1),
                showlegend = TRUE
            ) |>
            plotly$add_trace(
                type = "scatterpolar",
                r = r2,
                theta = theta,
                mode = "lines+markers",
                name = name2,
                line = list(color = color2, width = 2),
                marker = list(color = color2, size = 6),
                fill = "toself",
                fillcolor = paste0(color2, "26"),
                hoverinfo = "text",
                hovertext = sprintf("%s - %s: %.1f", name2, theta, r2),
                showlegend = TRUE
            ) |>
            plotly$layout(
                polar = list(
                    bgcolor = "rgba(0,0,0,0)",
                    gridshape = "linear",
                    radialaxis = list(
                        visible = TRUE,
                        range = c(0, 20),
                        showgrid = FALSE,
                        showticklabels = FALSE,
                        showline = FALSE
                    ),
                    angularaxis = list(
                        rotation = 90,
                        direction = "clockwise",
                        gridcolor = "rgba(255,255,255,0.3)",
                        linecolor = "rgba(255,255,255,0.3)",
                        tickfont = list(color = "#f2f2f2", size = 12, family = radarFontFamily)
                    )
                ),
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                font = list(family = radarFontFamily, color = "#f2f2f2"),
                showlegend = TRUE,
                legend = list(
                    orientation = "h",
                    x = 0.5,
                    xanchor = "center",
                    y = -0.1,
                    font = list(color = "#f2f2f2")
                ),
                # Generous on purpose - category labels extend past the circle
                margin = list(l = 90, r = 90, t = 30, b = 90)
            ) |>
            plotly$config(displayModeBar = FALSE, responsive = TRUE)

    }

    # Used instead of the overlaid chart when the two players aren't the same type
    buildSingleRadar <- function(playerRow, color, name) {

        groups <- if (playerRow$pos_gk == 20) keeperRadarGroups else outfieldRadarGroups
        categories <- names(groups)

        values <- sapply(groups, function(attrs) getRadarCategoryValue(playerRow, attrs))

        theta <- c(categories, categories[1])
        r <- c(values, values[1])

        radar <- plotly$plot_ly()

        for (i in seq_along(radarLevels)) {
            radar <- radar |>
                plotly$add_trace(
                    type = "scatterpolar",
                    r = rep(radarLevels[i], length(theta)),
                    theta = theta,
                    mode = "lines",
                    line = list(
                        color = sprintf("rgba(255,255,255,%.2f)", radarRingOpacities[i]),
                        width = 1
                    ),
                    fill = "none",
                    hoverinfo = "skip",
                    showlegend = FALSE
                )
        }

        radar |>
            plotly$add_trace(
                type = "scatterpolar",
                r = r,
                theta = theta,
                mode = "lines+markers",
                name = name,
                line = list(color = color, width = 2),
                marker = list(color = color, size = 6),
                fill = "toself",
                fillcolor = paste0(color, "26"),
                hoverinfo = "text",
                hovertext = sprintf("%s: %.1f", theta, r),
                showlegend = FALSE
            ) |>
            plotly$layout(
                polar = list(
                    bgcolor = "rgba(0,0,0,0)",
                    gridshape = "linear",
                    radialaxis = list(
                        visible = TRUE,
                        range = c(0, 20),
                        showgrid = FALSE,
                        showticklabels = FALSE,
                        showline = FALSE
                    ),
                    angularaxis = list(
                        rotation = 90,
                        direction = "clockwise",
                        gridcolor = "rgba(255,255,255,0.3)",
                        linecolor = "rgba(255,255,255,0.3)",
                        tickfont = list(color = "#f2f2f2", size = 12, family = radarFontFamily)
                    )
                ),
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                font = list(family = radarFontFamily, color = "#f2f2f2"),
                showlegend = FALSE,
                title = list(text = paste0("<b>", name, "</b>"), font = list(size = 13, color = "#f2f2f2")),
                margin = list(l = 85, r = 85, t = 45, b = 40)
            ) |>
            plotly$config(displayModeBar = FALSE, responsive = TRUE)

    }

    # Same type -> one overlaid chart, different types -> two side by side
    samePositionType <- shiny$reactive({

        shiny$req(player1(), player2())

        (player1()$pos_gk == 20) == (player2()$pos_gk == 20)

    })

    output$comparisonRadar <- plotly$renderPlotly({

        shiny$req(player1(), player2(), player1Color(), player2Color())
        shiny$req(samePositionType())

        buildComparisonRadar(
            player1(), player2(),
            player1Color(), player2RadarColor(),
            player1()$name, player2()$name
        )

    })

    output$comparisonRadarPlayer1 <- plotly$renderPlotly({

        shiny$req(player1(), player1Color())
        shiny$req(!samePositionType())

        buildSingleRadar(player1(), player1Color(), player1()$name)

    })

    output$comparisonRadarPlayer2 <- plotly$renderPlotly({

        shiny$req(player2(), player2Color())
        shiny$req(!samePositionType())

        buildSingleRadar(player2(), player2Color(), player2()$name)

    })


    #### COMPARISON ####

    getSecondaryPositions <- function(p) {

      posCols <- p[grepl("^pos_", names(p))]
      posValues <- unlist(posCols)
      posNames <- toupper(sub("^pos_", "", names(posCols)))

      secondary <- posNames[posValues >= 10 & posValues < 20]

      if (length(secondary) == 0) "—" else paste(secondary, collapse = ", ")

    }

    buildPlayerCard <- function(p, color) {

      shiny$div(

        class = "player-info-panel",

        shiny$div(
          style = paste0(
            "height:4px; width:56px; background:", color, ";
            margin: 0 auto 12px auto;
            border-radius: 999px;"
          )
        ),

        shiny$div(
          style = "text-align:center;",
          linkOrganization(
            p$team,
            onlyImg = TRUE,
            height = 100
          )
        ),

        shiny$div(
          style = "text-align:center; margin:10px 0 2px 0;",

          shiny$div(
            shiny$tags$a(href = paste0("#!/tracker/player?pid=", p$pid),
            p$name,
            style = "font-size:20px; font-weight:800; letter-spacing:0.2px;"
          )),

          shiny$div(
            style = paste0(
              "height:3px; width:56px; margin:6px auto 0 auto;
              border-radius:999px; background:", color, ";"
            )
          )

        ),

        shiny$div(
          style = "
            display:flex;
            justify-content:center;
            gap:8px;
            flex-wrap:wrap;
            margin: 10px 0 12px 0;
          ",

          shiny$span(
            p$team,
            style = "
              font-size:11px;
              font-weight:700;
              text-transform:uppercase;
              letter-spacing:0.6px;
              padding:4px 10px;
              border-radius:999px;
              background:var(--bottom-background);
            "
          ),

          shiny$span(
            p$position,
            style = paste0(
              "font-size:11px;
              font-weight:700;
              text-transform:uppercase;
              letter-spacing:0.6px;
              padding:4px 10px;
              border-radius:999px;
              color:white;
              background:", color, ";"
            )
          )

        ),

        shiny$div(
          style = "
            display:grid;
            grid-template-columns: 1fr 1fr;
            gap: 6px 12px;
            font-size:13px;
            padding: 0 6px;
            margin-bottom: 10px;
          ",

          shiny$div(shiny$strong("TPE: "), p$tpe),
          shiny$div(shiny$strong("Class: "), p$class),
          shiny$div(shiny$strong("Nationality: "), p$nationality),
          shiny$div(shiny$strong("Secondary Pos: "), getSecondaryPositions(p))

        )

      )

    }

    output$comparison <- shiny$renderUI({

      if (
        is.null(input$player1) || input$player1 == "" ||
        is.null(input$player2) || input$player2 == ""
      ) {

        return(
          shiny$div(
            style = "text-align:center; padding: 48px 20px; opacity:0.7;",
            shiny$icon("users", style = "font-size:38px; margin-bottom:10px; display:block;"),
            shiny$h5("Select two players above to start comparing"),
            shiny$p(
              "Once you've picked a player in each dropdown, their attributes and career stats ",
              "will show up here.",
              class = "text-muted"
            )
          )
        )

      }

      shiny$req(
        player1(),
        player2(),
        careerPlayer1(),
        careerPlayer2()
      )

      p1 <- player1()
      p2 <- player2()

      #### PLAYER CARDS ####

      shiny$div(
        class = "player-compare-row",

        shiny$div(
          class = "player-compare-col",
          buildPlayerCard(p1, player1Color())
        ),

        shiny$div(
          class = "player-compare-col",
          buildPlayerCard(p2, player2Color())
        )

      )

    })

  })

}