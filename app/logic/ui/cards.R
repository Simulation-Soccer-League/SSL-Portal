box::use(
  bslib,
  dplyr[if_else],
  shiny,
  stringr[str_replace_all],
)

box::use(
  app/logic/ui/reactableHelper[linkOrganization],
)

#' @export
resultCard <- function(data, i) {
  bslib$card(
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
      shiny$h4(paste(data[i, "HomeScore"], data[i, "AwayScore"], sep = "-") |>
                 str_replace_all(pattern = "NA", replacement = " "))
    ),
    bslib$card_footer(
      paste(
        paste(
          data[i, "Matchtype"],
          data[i, "Matchday"],
          sep = ", "
        ),
        paste(
          data[i, "IRLDate"]
        ),
        sep = "<br>"
      ) |>
        shiny$HTML() |>
        shiny$div(align = "center")
    )
  )
}
