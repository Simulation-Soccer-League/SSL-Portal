box::use(
  shiny
)

box::use(
  app/logic/constant,
)


#' @export
infoBox <- function(header, value) {
  shiny$div(
    style = "
      padding: 5px;
      border-radius: 8px;
      box-shadow: 2px 2px 4px var(--middle-background);
    ",
    shiny$div(
      style = "
        font-weight: 400; 
        font-size: 1.2rem; 
        line-height: 140%; 
        white-space: nowrap;",
      header
    ),
    shiny$div(
      style = "
        font-size: 1.4rem; 
        font-weight: 600; 
        display: flex; 
        align-items: center; 
        gap 0.4rem;",
      value
    )
  )
}

