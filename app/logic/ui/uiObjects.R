box::use(
  shiny
)

box::use(
  app/logic/constant,
)


#' @export
infoBox <- function(header, value) {
  shiny$div(
    shiny$div(
      style = "font-weight: 400; font-size: 1.2rem; line-height: 140%; white-space: nowrap;",
      header
    ),
    shiny$div(
      style = "font-size: 1.4rem; font-weight: 600; display: flex; align-items: center; gap 0.4rem;",
      value
    )
  )
}

