box::use(
  shinycssloaders[withSpinner],
)

#' @export
withSpinnerCustom <- function(x, height, caption = "Loading information!") {
  withSpinner(
    ui_element = x, 
    proxy.height = paste0(height, "px"), 
    type = 6, 
    caption = caption,
    color = "white",
    size = 0.5
  )
}
