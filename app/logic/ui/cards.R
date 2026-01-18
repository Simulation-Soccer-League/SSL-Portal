box::use(
  bslib,
  dplyr[if_else],
  shiny,
  stringr[str_replace_all],
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)
# Division and Matchday parsing
parse_division_matchday <- function(match_day) {

  # New format: "1.2", "2.4"
  if (grepl("^\\d+\\.\\d+$", match_day)) {
    parts <- strsplit(match_day, "\\.")[[1]]
    return(list(
      division = parts[1],
      matchday = parts[2]
    ))
  }

  # Old format: "1", "2", etc. (≤ S23)
  list(
    division = NULL,
    matchday = match_day
  )
}


# Competition key (used for colour + logo)

get_competition_key <- function(match_type, match_day) {

  parsed <- parse_division_matchday(match_day)

  # SSL Shield (Cup + Shi)
  if (match_type == 0 && match_day == "Shi") {
    return("shield")
  }

  # Majors (S24+)
  if (match_type == 1 && !is.null(parsed$division)) {
    return(paste0("major-div", parsed$division))
  }

  # Minors (S24+)
  if (match_type == 2 && !is.null(parsed$division)) {
    return(paste0("minor-div", parsed$division))
  }

  # Legacy Majors / Minors (≤ S23)
  if (match_type == 1) return("major")
  if (match_type == 2) return("minor")

  # Others
  if (match_type == 0) return("cup")
  if (match_type == 5) return("wsfc")

  "friendlies"
}


# Stage label (Footer text)
get_stage_label <- function(match_type, match_day) {

  # Majors / Minors
if (match_type %in% c(1, 2)) {

  parsed <- parse_division_matchday(match_day)

  if (!is.null(parsed$division)) {
    return(
      paste(
        "D", parsed$division,
        " MD", parsed$matchday
      )
    )
  }

  return(paste("MD", match_day))
}
  # SSL Shield
  if (match_type == 0 && match_day == "Shi") {
    return("Shield")
  }

  # Cup (non-shield)
  if (match_type == 0) {
    return(
      switch(
        as.character(match_day),
        "Q"   = "Qualifying",
        "R16" = "Round of 16",
        "QF"  = "Quarter Final",
        "SF"  = "Semi Final",
        "F"   = "Final",
        match_day
      )
    )
  }

  # WSFC
  if (match_type == 5) {

    # Pre-season
    if (match_day == "Pre") {
      return("Pre Season")
    }

    # Group stages: A1, B2, C3, D4
    if (grepl("^[A-D][0-9]+$", match_day)) {
      group <- substr(match_day, 1, 1)
      round <- substr(match_day, 2, nchar(match_day))
      return(paste("Group ", group, " MD", round))
    }

    # Knockouts
    return(
      switch(
        as.character(match_day),
        "QF" = "Quarter Final",
        "SF" = "Semi Final",
        "F"  = "Final",
        match_day
      )
    )
  }

  # Friendlies
  return(
    switch(
      as.character(match_day),
      "Pre" = "Pre Season",
      "Friendly"
    )
  )
}

#Result Card
#' @export
resultCard <- function(data, i) {

  competition_key <- get_competition_key(
    data[i, "MatchType"],
    data[i, "MatchDay"]
  )

  # Build logo path INSIDE the function
  competition_logo <- paste0(
    "/static/competition/",
    gsub("-", "_", competition_key),
    ".png"
  )

  stage_label <- get_stage_label(
    data[i, "MatchType"],
    data[i, "MatchDay"]
  )

  card <- bslib$card(
    shiny$div(
    class="result-card-inner",

    bslib$card_header(
      shiny$div(
        shiny$div(
          style = "display: inline-block; width: 40px;",
          linkOrganization(data[i, "Home"], onlyImg = TRUE, height = 40)
        ),
        shiny$strong(" - "),
        shiny$div(
          style = "display: inline-block; width: 40px;",
          linkOrganization(data[i, "Away"], onlyImg = TRUE, height = 40)
        ),
        align = "center"
      )
    ),

    bslib$card_body(
      shiny$h4(
        paste(data[i, "HomeScore"], data[i, "AwayScore"], sep = "-") |>
          str_replace_all(pattern = "NA", replacement = " ")
      )
    ),

    bslib$card_footer(
      shiny$div(
        align = "center",
        shiny$div(stage_label),
        shiny$div(data[i, "IRLDate"])
      )
    )
    ),

    # ✅ OVERLAY MUST BE INSIDE THE CARD
    shiny$div(
      class = "competition-overlay",
      shiny$img(
        src = competition_logo,
        alt = paste("Competition:", competition_key)
      )
    )
  
  )

  shiny::tagAppendAttributes(
    card,
    class = "result-card",
    `data-league` = competition_key
  )
}
