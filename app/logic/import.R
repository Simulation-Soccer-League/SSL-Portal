box::use(
  dplyr[
    across, 
    as_tibble, 
    case_when, 
    if_else, 
    select, 
    mutate, 
    rename, 
    relocate
  ],
  rvest[read_html, html_elements, html_table],
  stringr[
    str_detect,
    str_remove_all,
    str_replace, 
    str_replace_all,
    str_split, 
    str_squish,
    str_to_lower, 
    str_to_title,
    str_to_upper,
  ]
)

box::use(
  app/logic/constant,
  app/logic/db/database[indexQuery]
)

#' @export
parseFMdata <- function(path){
  FM <- 
    read_html(path, encoding = "UTF-8") |> 
    html_elements("table") |> 
    html_table()
    
  FM <- 
    FM[[1]] |> 
    mutate(
      Name = 
        Name |> 
        str_split(
          pattern = " - ", 
          simplify = TRUE
        ) |> 
        as_tibble() |> 
        select(1) |> 
        unlist()
    ) |> 
    select(
      -"Inf",
      ## FM24 does not have Rec as an automatic column
      # -"Rec"
    ) |> 
    mutate(
      Name = 
        case_when(
          str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", 
          # str_detect(Name, "Liang") ~ "Kuai Liang",
          # str_detect(Name, "Princess") ~ "Princess Changshan",
          TRUE ~ Name)
    ) |> 
    relocate(
      c(Pun, Ref, TRO),
      .after = `1v1`
    ) |> 
    rename(
      Apps = Apps,
      `Minutes Played` = Mins,
      `Distance Run (km)` = Distance,
      `Average Rating` = `Av Rat`,
      `Player of the Match` = `PoM`,
      Goals = Gls,
      Assists = Ast,
      `xG Overperformance` = `xG-OP`,
      
      `Shots on Target` = ShT,
      
      `Blocks` = Blk,
      
      `Penalties Taken` = Pens,
      `Penalties Scored` = `Pens S`,
      
      `Attempted Passes` = `Pas A`,
      `Successful Passes` = `Ps C`,
      `Key Passes` = `K Pas`,
      `Open Play Key Passes` = `OP-KP`,
      
      `Successful Open Play Crosses` = `OP-Crs C`,
      `Attempted Open Play Crosses` = `OP-Crs A`,
      `Successful Crosses` = `Cr C`,
      `Attempted Crosses` = `Cr A`,
      
      `Chances Created` = CCC,
      
      `Successful Headers` = Hdrs,
      `Attempted Headers` = `Hdrs A`,
      `Header%` = `Hdr %`,
      `Key Headers` = `K Hdrs`,
      
      Dribbles = `Drb`,
      
      `Attempted Tackles` = `Tck A`,
      `Tackles Won` = `Tck C`,
      `Tackle%` = `Tck R`,
      `Key Tackles` = `K Tck`,
      
      Interceptions = Itc,
      `Shots Blocked` = `Shts Blckd`,
      Clearances = Clear,
      `Mistakes Leading to Goals` = `Gl Mst`,
      `Yellow Cards` = Yel,
      `Red Cards` = Red,
      Fouls = Fls,
      `Fouls Against` = FA,
      Offsides = Off,
      
      `Progressive Passes` = `Pr Passes`,
      
      `Successful Presses` = `Pres C`,
      `Attempted Presses` = `Pres A`,
      
      Drawn = D,
      Conceded = Conc,
      `Saves Parried` = Svp,
      `Saves Held`= Svh,
      `Saves Tipped` = Svt,
      `Penalties Saved` = `Pens Saved`,
      `Penalties Faced` = `Pens Faced`,
      # `Clean Sheets` = `Clean sheets`
      `Clean Sheets` = Shutouts,
      `xSave%`= `xSv %`,
      `xG Prevented` = xGP
    ) |> 
    mutate(
      across(
        c(
          `Minutes Played`:`Wor`,
          `Won`:`xG Prevented`
        ),
        .fns = str_replace_all,
        pattern = "[^-\\d\\.]+",
        replacement = ""
      ),
      Club = 
        case_when(
          Club == "Accra FC" ~ "Adowa Accra FC",
          Club == "São Paulo" ~ "União São Paulo",
          Club == "Red Star" ~ "Red Star Laos",
          Club == "E. Europe" ~ "Eastern Europe",
          Club == "Walland" ~ "Cymru",
          Club %in% c("Reykjavik U.", "Reykjavik") ~ "Reykjavik United",
          Club %in% c("Montréal U.", "Montréal") ~ "Montréal United",
          Club == "North Shore" ~ "North Shore United",
          Club == "Football Club de Rio" ~ "FC Rio",
          Club == "Alps" ~ "Alpen",
          Club == "Pyrénees" ~ "Pyrenees",
          Club == "Central America Caribbean" ~ "Central America",
          Club == "Eastern Europe" ~ "East Europe",
          TRUE ~ Club
        )
    ) |> 
    mutate(
      # Club =
      #   Club |>
      #   str_split(pattern = "-", simplify = TRUE) |> 
      #   select(1) |> 
      #   unlist() |> 
      #   str_squish(),
      `Left Foot` = 
        case_when(
          `Left Foot` == "Very Strong" ~ 20,
          `Left Foot` == "Strong" ~ 15,
          TRUE ~ 10
        ),
      `Right Foot` = 
        case_when(
          `Right Foot` == "Very Strong" ~ 20,
          `Right Foot` == "Strong" ~ 15,
          TRUE ~ 10
        )
    ) |> 
    mutate(
      across(
        !contains(
          c("Name", "Information", "Nationality", "Position", "Club")
        ),
        as.numeric
      ),
      `Pass%` = 
        (`Successful Passes` |> as.numeric()/
           `Attempted Passes` |> as.numeric()) |> 
        round(4)*100,
      `Cross%` = 
        (`Successful Crosses` |> as.numeric()/
           `Attempted Crosses` |> as.numeric()) |> 
        round(4)*100,
      `Save%` = 
        ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/
           (`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) |> 
        round(4) * 100,
    ) |> 
    suppressWarnings()
}

#' @export
importGameData <- function(data){
  keeper <- data$k |>
    mutate(
      across(
        name:club,
        ~ paste0('"', .x, '"')
      )
    )
  
  outfield <- data$o |>
    mutate(
      across(
        name:position,
        ~ paste0('"', .x, '"')
      )
    )
  
  outfieldPaste <- do.call(function(...) paste(..., sep = ", "), args = outfield)
  keeperPaste <- do.call(function(...) paste(..., sep = ", "), args = keeper)
  
  indexQuery(
    paste(
      "INSERT INTO gamedataoutfield VALUES ",
      paste0("(", outfieldPaste, ")", collapse = ", "),
      ";"
    )
  )
  
  indexQuery(
    paste(
      "INSERT INTO gamedatakeeper VALUES ",
      paste0("(", keeperPaste, ")", collapse = ", "),
      ";"
    )
  )
}