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
  embed <- sprintf("author: {
          name: 'THIS IS A TEST!'
        },
        title: 'TPE List',
        fields: [
                   {name: '', value:'```Test\\\\nTEST```'}
        ]")
  
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
          
          
          embed <- sprintf(
            "author: {name: 'A new PT has been graded!'},
              title: '%s',
              fields: [%s],
              footer: {
                text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
              }", 
            x,
            paste0(
              "{name: '', 
                  value: '", sprintf("```%s```", strings), "'}",
              collapse = ","
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
          
          embed <- sprintf(
            "author: {name: 'A new PT has been graded!'},
              title: '%s',
              fields: [%s],
              footer: {
                text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
              }", 
            x,
            sprintf("```%s```", gradedString)
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
  embed <- sprintf(
    "author: {
      name: 'The Academy Index has been updated!'
      },
      title: 'Season %s'", 
    season
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
  embed <- sprintf(
    "author: {
      name: %s 
      },
      title: 'Season %s'",
    sprintf("'The Index has been updated with %s!'", matchday),
    season
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
  embed <- sprintf(
    "author: {
        name: 'A new player has been created'
      },
      title: '%s',
      fields: [
         {
            name: 'TPE Banked',
            value: %s,
         },
         {
            name: 'Position',
            value: '%s',
         },
         { 
            name: 'Username',
            value: '%s',
         },
         { 
            name: 'Discord',
            value: '%s',
         },
      ]",
    paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'"),
    data$tpebank,
    data$position,
    username,
    getUserInformation(data$uid)$discord,
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
  embed <- sprintf(
    "author: {
        name: 'A new player has been created'
      },
      title: '%s',
      fields: [
         {
            name: 'TPE Banked',
            value: %s,
         },
         {
            name: 'Position',
            value: '%s',
         },
         { 
            name: 'Username',
            value: '%s',
         },
         { 
            name: 'Discord',
            value: '%s',
         },
      ]",
    paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'"),
    data$tpebank,
    data$position,
    username,
    getUserInformation(data$uid)$discord,
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
  embed <- sprintf(
    "author: {
        name: 'A player has retired'
      },
      title: '%s',
      fields: [
         {
            name: 'Team',
            value: %s,
         },
         { 
            name: 'Username',
            value: '%s',
         },
         { 
            name: 'Discord',
            value: '%s',
         },
      ]",
    paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'"),
    data$team,
    data$username,
    getUserInformation(data$uid)$discord,
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