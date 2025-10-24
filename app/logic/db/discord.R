## Function for queries to mybb
box::use(
  dplyr,
  shinyjs[runjs],
  stringr[str_remove_all],
)

player <- Sys.getenv("dscplayer")
tpe <- Sys.getenv("dsctpe")
approve <- Sys.getenv("dscapprove")
index <- Sys.getenv("dscindex")

#' @export
sendTest <- function(){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'THIS IS A TEST!'
        },
        title: 'TPE List',
        fields: [
                   {name: '', value:'```Test\\\\nTEST```'}
        ]
      }

      var params = {
        username: 'Dev Portal',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

#' @export
sendGradedTPE <- function(data){
  
  gradedString <- 
    apply(
      data |> 
        dplyr$select(username, tpe, source), 
      1, 
      function(row) paste(row, collapse = ": ")
    ) |> 
    paste(collapse = "\\n")
  
  if(nchar(gradedString) > 1024){
    result <- 
      apply(
        tpe |> 
          dplyr$select(username, tpe, source), 
        1, 
        function(row) paste(row, collapse = ": ")
      )
    
    splits <- nchar(gradedString) %/% 1024 + 1
    
    group <- rep(1:splits, length.out = length(result))
    
    splitResult <- split(result, group)
    
    strings <- lapply(splitResult, function(x) x |> paste(collapse = "\\n")) |> 
      unlist()
    
    jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", tpe, "');
      request.setRequestHeader('Content-type', 'application/json');
      var myEmbed = {
        author: {name: 'A new PT has been graded!'},
        title: 'User: TPE: Source',
        fields: [", 
                     paste0(
                       "{name: '', 
              value: '", sprintf("```%s```", strings), "'}",
                       collapse = ","
                     ),
                     "],
        footer: {
          text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
        } 
      };
      
      var params = {username: 'PT Watcher',embeds: [ myEmbed ]};
      
      request.send(JSON.stringify(params));
    }
    
    sendMessage();"
    )
    
    # cat(jscode)
    
    runjs(jscode)
    
  } else {
    jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", tpe, "');
      request.setRequestHeader('Content-type', 'application/json');
      var myEmbed = {
        author: {name: 'A new PT has been graded!'},
        title: 'User: TPE: Source',
        fields: [ 
          {name: '', 
           value: '", sprintf("```%s```", gradedString), "'}
        ],
        footer: {
          text: 'If you have received 0 or reduced TPE, please check a summary post in the PT thread. \\n\\nThe TPE has already been added to your player page, this is just a report.'
        } 
      };
      
      var params = {username: 'PT Watcher',embeds: [ myEmbed ]};
      
      request.send(JSON.stringify(params));
    }
    
    sendMessage();"
    )
    
    # cat(jscode)
    
    runjs(jscode)
  }
}

#' @export
sendAcademyIndexUpdate <- function(season){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", indexUpdate, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'The Academy Index has been updated!'
        },
        title: 'Season ", season, "'
      }

      var params = {
        username: 'Index Update',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

#' @export
sendIndexUpdate <- function(season){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", indexUpdate, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'The Index has been updated!'
        },
        title: 'Season ", season, "'
      }

      var params = {
        username: 'Index Update',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

#' @export
sendNewCreate <- function(data, username){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new player has been created'
        },
        title: '", paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'TPE Banked',
                      value: ", data$tpebank,",
                   },
                   {
                      name: 'Position',
                      value: '", data$position,"',
                   },
                   { 
                      name: 'Username',
                      value: '", username, "',
                   },
        ]
      }

      var params = {
        username: 'Captain Hook',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

#' @export
sendApprovedCreate <- function(data){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", approve, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A new player has been approved'
        },
        title: '", paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'TPE Banked',
                      value: ", data$tpebank,",
                   },
                   {
                      name: 'Position',
                      value: '", data$position,"',
                   },
                   { 
                      name: 'Username',
                      value: '", data$username, "',
                   },
                   { 
                      name: 'Discord',
                      value: '", data$discord, "',
                   },
        ]
      }

      var params = {
        username: 'Captain Hook',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}

#' @export
sendRetiredPlayer <- function(data){
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", player, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A player has retired'
        },
        title: '", paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'Team',
                      value: '", data$team,"',
                   },
                   { 
                      name: 'Username',
                      value: '", data$username, "',
                   },
        ]
      }

      var params = {
        username: 'Captain Hook',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
  
  jscode <- paste0("
    function sendMessage() {
      const request = new XMLHttpRequest();
      request.open('POST', '", approve, "');

      request.setRequestHeader('Content-type', 'application/json');

      var myEmbed = {
        author: {
          name: 'A player has retired'
        },
        title: '", paste0(data$first, " ", data$last) |> str_remove_all(pattern = "'") , "',
        fields: [
                   {
                      name: 'Team',
                      value: '", data$team,"',
                   },
                   { 
                      name: 'Username',
                      value: '", data$username, "',
                   },
        ]
      }

      var params = {
        username: 'Captain Hook',
        embeds: [ myEmbed ]
      }

      request.send(JSON.stringify(params));
    }
    sendMessage();
  ")
  
  runjs(jscode)
}