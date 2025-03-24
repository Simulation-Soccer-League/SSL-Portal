## Verifies the password from a response from the mybb db and the given password
box::use(
  digest,
  stringr[str_split],
  stringi[stri_rand_strings],
  lubridate[now, hours],
)

box::use(
  app/logic/db/database[mybbQuery, portalQuery],
)

#' Helper function to set the token
#' @export
setRefreshToken <- function(uid, token, session = shiny::getDefaultReactiveDomain()) {
  expires <- (now() + hours(72)) |> as.numeric()

  portalQuery({
    paste("INSERT INTO refreshtokens (uid, expires_at, token)
              VALUES (", uid, ",", expires, ", ", paste0("'", token, "'"), ")
              ON DUPLICATE KEY UPDATE token=", paste0("'", token, "'"), ", expires_at=", expires)
  })
}


#' @export
customCheckCredentials <- function(user, password, session = shiny::getDefaultReactiveDomain()) {
  res <-
    mybbQuery(
      query =
        paste(
          "SELECT uid, username, password, salt, usergroup, additionalgroups, suspendposting
        FROM mybb_users
        WHERE username = '", user, "'",
          sep = ""
        )
    ) |> 
    suppressWarnings()
  
  if(nrow(res) == 1){
    saltedPASS <- 
      paste(
        digest$digest(
          res$salt, 
          algo = "md5", 
          serialize = FALSE
        ), 
        digest$digest(
          password, 
          algo = "md5", 
          serialize = FALSE
        ), 
        sep = "") |> 
      digest$digest(algo = "md5", serialize = FALSE)
    
    if(saltedPASS != res$password) {
      list(result = FALSE)
    } else{
      token <- paste0(replicate(n = 4, expr = stri_rand_strings(1, length = 4)), collapse = "-")
      
      setRefreshToken(uid = res$uid, token = token)
      
      msg <- list(
        name = "token", value = token
      )
      
      session$sendCustomMessage("cookie-set", msg)
      
      list(
        result = TRUE, 
        userInfo = 
          list(
            uid = res$uid, 
            username = res$username, 
            usergroup = 
              paste(res$usergroup, res$additionalgroups, sep = ",") |> 
              str_split(pattern = ",", simplify = TRUE) |>
              as.numeric() |> 
              as.list(),
            suspended = res$suspendposting == 1
          )
      )
    }
    
  } else {
    list(result = FALSE,
         userInfo = 
           list(
             uid = NULL, 
             username = NULL, 
             usergroup = NULL,
             suspended = NULL
           ))
  }
}

#' @export
getRefreshToken <- function(token){
  portalQuery(
    paste("SELECT rt.*, mb.username, mb.usergroup, mb.additionalgroups, mb.suspendposting 
              FROM refreshtokens rt 
              JOIN mybbdb.mybb_users mb ON rt.uid = mb.uid 
              WHERE rt.token = '", token, "';", sep = "")
  ) |> 
    suppressWarnings()
}

#' Checks if the user is awaiting activation, banned or suspended
#' @export
isNonActiveForumUser <- function(usergroup, suspended){
  any(c(0,5, 7) %in% usergroup) | suspended
}

#' @export
isBoD <- function(usergroup){
  any(c(3, 4) %in% usergroup)
}

#' @export
isBoDIntern <- function(usergroup){
  15 %in% usergroup
}

#' @export
isBankerAccountant <- function(usergroup){
  12 %in% usergroup
}

#' @export
isPT <- function(usergroup){
  11 %in% usergroup
}

#' @export
isManager <- function(usergroup){
  8 %in% usergroup
}

#' @export
isFileworker <- function(usergroup){
  14 %in% usergroup
}

