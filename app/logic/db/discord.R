## Function for queries to mybb
box::use(
  dplyr,
  httr[POST],
  purrr[map],
  stringr[str_remove_all],
)

box::use(
  app/logic/get/getUser[
    getUserInformation,
  ],
)

player <- Sys.getenv("dscplayer")
tpe <- Sys.getenv("dsctpe")
approve <- Sys.getenv("dscapprove")
index <- Sys.getenv("dscindex")

#' @export
sendTest <- function(){
  embed <- list(
    author = list(
      name = "THIS IS A TEST!"
    ),
    title = "TPE List",
    fields = list(
      list(
        name = "",
        value = "```Test\nTEST```"
      )
    )
  )
  
  
  POST(
    url = player,
    body = list(
      username = "Dev Portal",
      embeds = list(embed)
    ),
    encode = "json"
  )
}

#' @export
sendGradedTPE <- function(data){
  
  map(data$source |> unique(),
      .f = function(x){
        
        gradedString <- 
          apply(
            data |> 
              dplyr$filter(source == x) |> 
              dplyr$select(username, tpe) |> 
              dplyr$arrange(username), 
            1, 
            function(row) paste(row, collapse = " - ")
          ) |> 
          paste(collapse = "\\n")
        
        if(nchar(gradedString) > 1024){
          result <- 
            apply(
              data |> 
                dplyr$filter(source == x) |> 
                dplyr$select(username, tpe) |> 
                dplyr$arrange(username), 
              1, 
              function(row) paste(row, collapse = " - ")
            )
          
          splits <- nchar(gradedString) %/% 1024 + 1
          
          group <- rep(1:splits, length.out = length(result))
          
          splitResult <- split(result, group)
          
          strings <- lapply(splitResult, function(x) x |> paste(collapse = "\\n")) |> 
            unlist()
          
          
          embed <- list(
            author = list(
              name = "A new PT has been graded!"
            ),
            title = x,
            fields = lapply(
              strings,
              function(s) list(
                name = "",
                value = sprintf("```%s```", s)
              )
            ),
            footer = list(
              text = paste(
                "If you have received 0 or reduced TPE, please check a summary post in the PT thread.",
                "",
                "The TPE has already been added to your player page, this is just a report.",
                sep = "\n"
              )
            )
          )
          
          POST(
            url = tpe,
            body = list(
              username = "PT Watcher",
              embeds = list(embed)
            ),
            encode = "json"
          )
          
        } else {
          
          embed <- list(
            author = list(
              name = "A new PT has been graded!"
            ),
            title = x,
            fields = list(
              list(
                name = "",
                value = sprintf("```%s```", gradedString)
              )
            ),
            footer = list(
              text = paste(
                "If you have received 0 or reduced TPE, please check a summary post in the PT thread.",
                "",
                "The TPE has already been added to your player page, this is just a report.",
                sep = "\n"
              )
            )
          )
          
          POST(
            url = tpe,
            body = list(
              username = "PT Watcher",
              embeds = list(embed)
            ),
            encode = "json"
          )
        }
      })
  
}

#' @export
sendAcademyIndexUpdate <- function(season){
  embed <- list(
    author = list(
      name = "The Academy Index has been updated!"
    ),
    title = sprintf("Season %s", season)
  )
  
  POST(
    url = index,
    body = list(
      username = "Index Update",
      embeds = list(embed)
    ),
    encode = "json"
  )
  
}

#' @export
sendIndexUpdate <- function(matchday, season){
  embed <- list(
    author = list(
      name = sprintf("'The Index has been updated with %s!'", matchday)
    ),
    title = paste("Season", season)
  )
  
  POST(
    url = index,
    body = list(
      username = "Index Update",
      embeds = list(embed)
    ),
    encode = "json"
  )
  
}

#' @export
sendNewCreate <- function(data, username){
  embed <- list(
    author = list(
      name = "A new player has been created"
    ),
    title = paste0(data$first, " ", data$last),
    fields = list(
      list(
        name = "TPE Banked",
        value = as.character(data$tpebank),
        inline = TRUE
      ),
      list(
        name = "Position",
        value = data$position,
        inline = TRUE
      ),
      list(
        name = "Username",
        value = username,
        inline = TRUE
      ),
      list(
        name = "Render",
        value = data$render,
        inline = TRUE
      ),
      list(
        name = "Discord",
        value = getUserInformation(data$uid)$discord,
        inline = TRUE
      )
    )
  )
  
  POST(
    url = player,
    body = list(
      username = "Captain Hook",
      embeds = list(embed)
    ),
    encode = "json"
  )
}

#' @export
sendApprovedCreate <- function(data){
  embed <- list(
    author = list(
      name = "A new player has been created"
    ),
    title = paste0(data$first, " ", data$last),
    fields = list(
      list(
        name = "TPE Banked",
        value = as.character(data$tpebank),
        inline = TRUE
      ),
      list(
        name = "Position",
        value = data$position,
        inline = TRUE
      ),
      list(
        name = "Username",
        value = data$username,
        inline = TRUE
      ),
      list(
        name = "Discord",
        value = getUserInformation(data$uid)$discord,
        inline = TRUE
      )
    )
  )
  
  POST(
    url = approve,
    body = list(
      username = "Captain Hook",
      embeds = list(embed)
    ),
    encode = "json"
  )
}

#' @export
sendRetiredPlayer <- function(data){
  embed <- list(
    author = list(
      name = "A player has retired"
    ),
    title = paste0(data$first, " ", data$last) |>
      str_remove_all("'"),
    fields = list(
      list(
        name = "Team",
        value = as.character(data$team),
        inline = TRUE
      ),
      list(
        name = "Username",
        value = data$username,
        inline = TRUE
      ),
      list(
        name = "Discord",
        value = getUserInformation(data$uid)$discord,
        inline = TRUE
      )
    )
  )
  
  POST(
    url = player,
    body = list(
      username = "Captain Hook",
      embeds = list(embed)
    ),
    encode = "json"
  )
  
  POST(
    url = approve,
    body = list(
      username = "Captain Hook",
      embeds = list(embed)
    ),
    encode = "json"
  )
}