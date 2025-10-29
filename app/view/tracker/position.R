box::use(
  bslib,
  dplyr,
  ggplot2,
  grDevices[dev.off],
  magick,
  shiny,
  tidyr,
)

box::use(
  app / logic / constant,
  app / logic / db / get[getPlayers],
  app / logic / ui / spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          colwidths = c(2, 10),
          shiny$h1("Position Tracker"),
          ""
        )
      ),
      bslib$card_body(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 2fr"),
          shiny$div(
            shiny$p("The pitch shows the number of players that has the position 
                    as one of their Primary/Secondary positions."),
            shiny$radioButtons(
              ns("activeStatus"),
              "Show only active users?",
              choices = c("Yes", "No"),
              selected = "Yes"
            )
          ),
          shiny$imageOutput(
            ns("fieldImage"), 
            height = 600
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ### Data
    players <- shiny$reactive({
      data <- getPlayers(active = TRUE) |>
        dplyr$select(
          name, team, dplyr$contains("pos_"), class, userStatus
        ) 
      
      if (input$activeStatus == "Yes") {
        data |> 
          dplyr$filter(
            userStatus == "Active"
          )
      } else {
        data
      }
    }) |> 
      shiny$bindCache(id, input$activeStatus) |> 
      shiny$bindEvent(input$activeStatus) 
      
    
    positions <- shiny$reactive({
      
      players() |> 
        tidyr$pivot_longer(
          dplyr$contains("pos_"),
          names_to = "posExp",
          values_to = "Value"
        ) |> 
        cbind(
          constant$positionalCoord
        ) |> 
        dplyr$filter(
          Value != 0
        ) |> 
        dplyr$group_by(position) |> 
        dplyr$summarize(
          x = mean(x),
          y = mean(y),
          primary = sum(Value == 20),
          secondary = sum(Value == 15)
        ) |> 
        dplyr$ungroup()
    }) |> 
      shiny$bindCache(input$activeStatus)
    
    ### Output
    output$fieldImage <- shiny$renderImage({
      
      data <- positions()
      
      base <- constant$pitch |> 
        magick$image_ggplot()
      
      p <- 
        base + 
        ggplot2$geom_text(
          mapping = ggplot2$aes(x = x, y = y),
          data = data,
          label = data$primary,
          nudge_x = -35,
          size = 8,
          fontface = "bold"
        ) + 
        ggplot2$geom_text(
          mapping = ggplot2$aes(x = x, y = y),
          data = data,
          label = "/",
          size = 8,
          fontface = "bold"
        ) + 
        ggplot2$geom_text(
          mapping = ggplot2$aes(x = x, y = y),
          data = data,
          label = data$secondary,
          nudge_x = 35,
          size = 8,
          fontface = "bold"
        ) +
        ggplot2$geom_rect(
          ggplot2$aes(xmin = 60, xmax = 200, ymin = 100, ymax = 370),
          fill = NA,
          color = "black",
          linetype = 2
        ) + 
        ggplot2$geom_rect(
          ggplot2$aes(xmin = 550, xmax = 690, ymin = 100, ymax = 370),
          fill = NA,
          color = "black",
          linetype = 2
        ) +
        ggplot2$geom_rect(
          ggplot2$aes(xmin = 60, xmax = 200, ymin = 410, ymax = 680),
          fill = NA,
          color = "black",
          linetype = 2
        ) + 
        ggplot2$geom_rect(
          ggplot2$aes(xmin = 550, xmax = 690, ymin = 410, ymax = 680),
          fill = NA,
          color = "black",
          linetype = 2
        ) +
        ggplot2$geom_text(
          mapping = ggplot2$aes(x = x, y = y),
          data = constant$positionalCoord,
          nudge_y = 35,
          label = constant$positionalCoord$position,
          size = 4,
          fontface = "italic",
          color = "#00044D"
        ) 
      
      card <- magick$image_graph(res = 96)
      print(
        p + 
          ggplot2$theme(
            legend.position = "none"
          )
      )
      dev.off()
      
      tempImage <- 
        card |> 
        magick$image_crop(geometry = "480x600+160") |> 
        magick$image_write(tempfile(fileext = "png"), format = "png")
      
      return(
        list(
          src = tempImage, 
          contentType = "image/png"
        )
      )
    }, 
      deleteFile=TRUE
    )
  })
}
