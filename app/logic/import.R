box::use(
  DBI, 
  dplyr[
    across, 
    as_tibble, 
    case_when, 
    mutate, 
    rename, 
    relocate,
    select,
  ],
  glue,
  rvest[read_html, html_elements, html_table],
  stringr[
    str_detect,
    str_replace_all,
    str_split, 
  ],
  
)

box::use(
  app/logic/db/database[createConnection],
)

#' @export
parseFMdata <- function(path) {
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
          Club %in% c("Alpine Federation", "Alps") ~ "Alpen",
          Club %in% c("Pyrénees", "Pyrenees Union") ~ "Pyrenees",
          Club %in% c("C. America & Caribbean", "Central America Caribbean") ~ "Central America",
          Club == "Eastern Europe" ~ "East Europe",
          Club == "Rapid Magyar" ~ "Rapid Magyar SC",
          Club == "Seoul Mythic FC" ~ "Seoul MFC",
          TRUE ~ Club
        )
    ) |> 
    mutate(
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
        (`Successful Passes` |> as.numeric() /
           `Attempted Passes` |> as.numeric()) |> 
        round(4) * 100,
      `Cross%` = 
        (`Successful Crosses` |> as.numeric() /
           `Attempted Crosses` |> as.numeric()) |> 
        round(4) * 100,
      `Save%` = 
        ((`Saves Parried` + `Saves Held` + `Saves Tipped`)/
           (`Saves Parried` + `Saves Held` + `Saves Tipped` + Conceded)) |> 
        round(4) * 100,
    ) |> 
    suppressWarnings()
}

#' @export
importGameData <- function(data) {
  con <- createConnection("index")
  
  outfield <- data$o
  keeper <- data$k
  
  DBI$dbExecute(con, "SET NAMES utf8mb4;")
  DBI$dbExecute(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbExecute(con, "SET character_set_connection=utf8mb4;")
  
  DBI$dbBegin(con)
  
  tryCatch({
    insert <- "INSERT INTO gamedataoutfield (
        name, club, position,
        acc, aer, agg, agi, ant, bal, bra, cmd, com, cmp, cnt, cor, cro, `dec`, det, dri, ecc,
        fin, fir, fla, fre, han, hea, jum, kic, ldr, lon, `l th`, mar, nat, otb, `1v1`,
        pac, pas, pen, `pos`, pun, `ref`, tro, sta, str, tck, tea, tec, thr, vis, wor,
        apps, `minutes played`, `distance run (km)`, `average rating`, `player of the match`,
        goals, assists, xg, xa, `shots on target`, shots, `penalties taken`, `penalties scored`,
        `successful passes`, `attempted passes`, `pass%`, `key passes`,
        `successful crosses`, `attempted crosses`, `cross%`, `chances created`,
        `successful headers`, `attempted headers`, `header%`, `key headers`,
        dribbles, `tackles won`, `attempted tackles`, `tackle%`, `key tackles`,
        interceptions, clearances, `mistakes leading to goals`, `yellow cards`, `red cards`,
        fouls, `fouls against`, offsides,
        `xg overperformance`, `goals outside box`, `fk shots`, blocks,
        `open play key passes`, `successful open play crosses`, `attempted open play crosses`,
        `shots blocked`, `progressive passes`, `successful presses`, `attempted presses`,
        gid
      )
      VALUES "
    
    values <- 
      glue$glue_sql(
        "(
        {name}, {club}, {position},
        {acc}, {aer}, {agg}, {agi}, {ant}, {bal}, {bra}, {cmd}, {com}, {cmp}, {cnt}, {cor}, {cro}, {dec}, {det}, {dri}, {ecc},
        {fin}, {fir}, {fla}, {fre}, {han}, {hea}, {jum}, {kic}, {ldr}, {lon}, {l_th}, {mar}, {nat}, {otb}, {one_v_one},
        {pac}, {pas}, {pen}, {pos}, {pun}, {ref}, {tro}, {sta}, {str}, {tck}, {tea}, {tec}, {thr}, {vis}, {wor},
        {apps}, {minutes_played}, {distance_run_km}, {average_rating}, {player_of_the_match},
        {goals}, {assists}, {xg}, {xa}, {shots_on_target}, {shots}, {penalties_taken}, {penalties_scored},
        {successful_passes}, {attempted_passes}, {pass_pct}, {key_passes},
        {successful_crosses}, {attempted_crosses}, {cross_pct}, {chances_created},
        {successful_headers}, {attempted_headers}, {header_pct}, {key_headers},
        {dribbles}, {tackles_won}, {attempted_tackles}, {tackle_pct}, {key_tackles},
        {interceptions}, {clearances}, {mistakes_leading_to_goals}, {yellow_cards}, {red_cards},
        {fouls}, {fouls_against}, {offsides},
        {xg_overperformance}, {goals_outside_box}, {fk_shots}, {blocks},
        {open_play_key_passes}, {successful_open_play_crosses}, {attempted_open_play_crosses},
        {shots_blocked}, {progressive_passes}, {successful_presses}, {attempted_presses},
        {gid}
      )",
        .con = con,
        name                 = outfield$name,
        club                 = outfield$club,
        position             = outfield$position,
        acc                  = outfield$acc,
        aer                  = outfield$aer,
        agg                  = outfield$agg,
        agi                  = outfield$agi,
        ant                  = outfield$ant,
        bal                  = outfield$bal,
        bra                  = outfield$bra,
        cmd                  = outfield$cmd,
        com                  = outfield$com,
        cmp                  = outfield$cmp,
        cnt                  = outfield$cnt,
        cor                  = outfield$cor,
        cro                  = outfield$cro,
        dec                  = outfield$dec,
        det                  = outfield$det,
        dri                  = outfield$dri,
        ecc                  = outfield$ecc,
        fin                  = outfield$fin,
        fir                  = outfield$fir,
        fla                  = outfield$fla,
        fre                  = outfield$fre,
        han                  = outfield$han,
        hea                  = outfield$hea,
        jum                  = outfield$jum,
        kic                  = outfield$kic,
        ldr                  = outfield$ldr,
        lon                  = outfield$lon,
        l_th                 = outfield$`l th`,
        mar                  = outfield$mar,
        nat                  = outfield$nat,
        otb                  = outfield$otb,
        one_v_one            = outfield$`1v1`,
        pac                  = outfield$pac,
        pas                  = outfield$pas,
        pen                  = outfield$pen,
        pos                  = outfield$pos,
        pun                  = outfield$pun,
        ref                  = outfield$ref,
        tro                  = outfield$tro,
        sta                  = outfield$sta,
        str                  = outfield$str,
        tck                  = outfield$tck,
        tea                  = outfield$tea,
        tec                  = outfield$tec,
        thr                  = outfield$thr,
        vis                  = outfield$vis,
        wor                  = outfield$wor,
        apps                 = outfield$apps,
        minutes_played       = outfield$`minutes played`,
        distance_run_km      = outfield$`distance run (km)`,
        average_rating       = outfield$`average rating`,
        player_of_the_match  = outfield$`player of the match`,
        goals                = outfield$goals,
        assists              = outfield$assists,
        xg                   = outfield$xg,
        xa                   = outfield$xa,
        shots_on_target      = outfield$`shots on target`,
        shots                = outfield$shots,
        penalties_taken      = outfield$`penalties taken`,
        penalties_scored     = outfield$`penalties scored`,
        successful_passes    = outfield$`successful passes`,
        attempted_passes     = outfield$`attempted passes`,
        pass_pct             = outfield$`pass%`,
        key_passes           = outfield$`key passes`,
        successful_crosses   = outfield$`successful crosses`,
        attempted_crosses    = outfield$`attempted crosses`,
        cross_pct            = outfield$`cross%`,
        chances_created      = outfield$`chances created`,
        successful_headers   = outfield$`successful headers`,
        attempted_headers    = outfield$`attempted headers`,
        header_pct           = outfield$`header%`,
        key_headers          = outfield$`key headers`,
        dribbles             = outfield$dribbles,
        tackles_won          = outfield$`tackles won`,
        attempted_tackles    = outfield$`attempted tackles`,
        tackle_pct           = outfield$`tackle%`,
        key_tackles          = outfield$`key tackles`,
        interceptions        = outfield$interceptions,
        clearances           = outfield$clearances,
        mistakes_leading_to_goals = outfield$`mistakes leading to goals`,
        yellow_cards         = outfield$`yellow cards`,
        red_cards            = outfield$`red cards`,
        fouls                = outfield$fouls,
        fouls_against        = outfield$`fouls against`,
        offsides             = outfield$offsides,
        xg_overperformance   = outfield$`xg overperformance`,
        goals_outside_box    = outfield$`goals outside box`,
        fk_shots             = outfield$`fk shots`,
        blocks               = outfield$blocks,
        open_play_key_passes = outfield$`open play key passes`,
        successful_open_play_crosses = outfield$`successful open play crosses`,
        attempted_open_play_crosses  = outfield$`attempted open play crosses`,
        shots_blocked            = outfield$`shots blocked`,
        progressive_passes       = outfield$`progressive passes`,
        successful_presses       = outfield$`successful presses`,
        attempted_presses        = outfield$`attempted presses`,
        gid                      = outfield$gid
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
    
    insert <- "INSERT INTO gamedatakeeper (
        name, club, apps, `minutes played`, 
        `average rating`, `player of the match`, 
        `clean sheets`, conceded, `saves parried`, `saves held`, 
        `saves tipped`, `save%`, `penalties faced`, `penalties saved`, 
        `xsave%`, `xg prevented`, gid
      )
      VALUES "
    
    values <- 
      glue$glue_sql(
        "(
        {name}, {club}, {apps}, {minutes_played}, 
        {average_rating}, {player_of_the_match}, 
        {clean_sheets}, {conceded}, {saves_parried}, {saves_held}, 
        {saves_tipped}, {save_pct}, {penalties_faced}, {penalties_saved}, 
        {xsave_pct}, {xg_prevented}, {gid}
      )",
        .con = con,
        name                = keeper$name,
        club                = keeper$club,
        apps                = keeper$apps,
        minutes_played      = keeper$`minutes played`,
        average_rating      = keeper$`average rating`,
        player_of_the_match = keeper$`player of the match`,
        clean_sheets        = keeper$`clean sheets`,
        conceded            = keeper$conceded,
        saves_parried       = keeper$`saves parried`,
        saves_held          = keeper$`saves held`,
        saves_tipped        = keeper$`saves tipped`,
        save_pct            = keeper$`save%`,
        penalties_faced     = keeper$`penalties faced`,
        penalties_saved     = keeper$`penalties saved`,
        xsave_pct           = keeper$`xsave%`,
        xg_prevented        = keeper$`xg prevented`,
        gid                 = keeper$gid
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
      
    DBI$dbCommit(con)
    
  }, error = function(e) {
    DBI$dbRollback(con)
    
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop(e$message)
  }, finally = {
    # Ensure the connection is closed
    DBI$dbDisconnect(con)
  })
  
}

#' @export
importAcademyData <- function(data) {
  outfield <- data$o
  keeper <- data$k
  
  con <- createConnection("index")
  
  DBI$dbExecute(con, "SET NAMES utf8mb4;")
  DBI$dbExecute(con, "SET CHARACTER SET utf8mb4;")
  DBI$dbExecute(con, "SET character_set_connection=utf8mb4;")
  
  DBI$dbBegin(con)
  
  DBI$dbExecute(
    con,
    glue$glue_sql(
      "DELETE FROM academyoutfield WHERE season = {season};",
      .con = con,
      season = outfield$season |> unique()
    )
  )
  
  DBI$dbExecute(
    con,
    glue$glue_sql(
      "DELETE FROM academykeeper WHERE season = {season};",
      .con = con,
      season = outfield$season |> unique()
    )
  )
  
  tryCatch({
    insert <- "INSERT INTO academyoutfield (
      season, name, club, position, apps, `minutes played`, `distance run (km)`,
      `average rating`, `player of the match`, goals, assists, xg, xa, 
      `xg overperformance`, `goals outside box`, `shots on target`, shots, 
      `fk shots`, blocks, `penalties taken`, `penalties scored`, 
      `successfull passes`, `attempted passes`, `pass%`, `key passes`, 
      `open play key passes`, `successful open play crosses`, 
      `attempted open play crosses`, `successful crosses`, `attempted crosses`, 
      `cross%`, `chances created`, `successful headers`, `attempted headers`, 
      `header%`, `key headers`, dribbles, `tackles won`, `attempted tackles`, 
      `tackle%`, `key tackles`, interceptions, `shots blocked`, clearances, 
      `mistakes leading to goals`, `yellow cards`, `red cards`, fouls, 
      `fouls against`, offsides, `progressive passes`, `successful presses`, 
      `attempted presses`
    )
    VALUES "
    
    values <- 
      glue$glue_sql(
        "(
          {season}, {name}, {club}, {position}, {apps}, {minutes_played}, {distance_run_km},
          {average_rating}, {player_of_the_match}, {goals}, {assists}, {xg}, {xa}, 
          {xg_overperformance}, {goals_outside_box}, {shots_on_target}, {shots}, 
          {fk_shots}, {blocks}, {penalties_taken}, {penalties_scored}, 
          {successful_passes}, {attempted_passes}, {pass_pct}, {key_passes}, 
          {open_play_key_passes}, {successful_open_play_crosses}, 
          {attempted_open_play_crosses}, {successful_crosses}, {attempted_crosses}, 
          {cross_pct}, {chances_created}, {successful_headers}, {attempted_headers}, 
          {header_pct}, {key_headers}, {dribbles}, {tackles_won}, {attempted_tackles}, 
          {tackle_pct}, {key_tackles}, {interceptions}, {shots_blocked}, {clearances}, 
          {mistakes_leading_to_goals}, {yellow_cards}, {red_cards}, {fouls}, 
          {fouls_against}, {offsides}, {progressive_passes}, {successful_presses}, 
          {attempted_presses}
        )",
        .con = con,
        season                   = outfield$season,
        name                     = outfield$name,
        club                     = outfield$club,
        position                 = outfield$position,
        apps                     = outfield$apps,
        minutes_played           = outfield$`minutes played`,
        distance_run_km          = outfield$`distance run (km)`,
        average_rating           = outfield$`average rating`,
        player_of_the_match      = outfield$`player of the match`,
        goals                    = outfield$goals,
        assists                  = outfield$assists,
        xg                       = outfield$xg,
        xa                       = outfield$xa,
        xg_overperformance       = outfield$`xg overperformance`,
        goals_outside_box        = outfield$`goals outside box`,
        shots_on_target          = outfield$`shots on target`,
        shots                    = outfield$shots,
        fk_shots                 = outfield$`fk shots`,
        blocks                   = outfield$blocks,
        penalties_taken          = outfield$`penalties taken`,
        penalties_scored         = outfield$`penalties scored`,
        successful_passes        = outfield$`successful passes`,
        attempted_passes         = outfield$`attempted passes`,
        pass_pct                 = outfield$`pass%`,
        key_passes               = outfield$`key passes`,
        open_play_key_passes     = outfield$`open play key passes`,
        successful_open_play_crosses = outfield$`successful open play crosses`,
        attempted_open_play_crosses  = outfield$`attempted open play crosses`,
        successful_crosses       = outfield$`successful crosses`,
        attempted_crosses        = outfield$`attempted crosses`,
        cross_pct                = outfield$`cross%`,
        chances_created          = outfield$`chances created`,
        successful_headers       = outfield$`successful headers`,
        attempted_headers        = outfield$`attempted headers`,
        header_pct               = outfield$`header%`,
        key_headers              = outfield$`key headers`,
        dribbles                 = outfield$dribbles,
        tackles_won              = outfield$`tackles won`,
        attempted_tackles        = outfield$`attempted tackles`,
        tackle_pct               = outfield$`tackle%`,
        key_tackles              = outfield$`key tackles`,
        interceptions            = outfield$interceptions,
        shots_blocked            = outfield$`shots blocked`,
        clearances               = outfield$clearances,
        mistakes_leading_to_goals = outfield$`mistakes leading to goals`,
        yellow_cards             = outfield$`yellow cards`,
        red_cards                = outfield$`red cards`,
        fouls                    = outfield$fouls,
        fouls_against            = outfield$`fouls against`,
        offsides                 = outfield$offsides,
        progressive_passes       = outfield$`progressive passes`,
        successful_presses       = outfield$`successful presses`,
        attempted_presses        = outfield$`attempted presses`
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
    
    insert <- "INSERT INTO academykeeper (
      season, name, club, position, apps, `minutes played`, 
      `average rating`, `player of the match`, won, lost, draw, 
      `clean sheets`, conceded, `saves parried`, `saves held`, 
      `saves tipped`, `save%`, `penalties faced`, `penalties saved`, 
      `xsave%`, `xg prevented`
    )
    VALUES "
    
    values <- 
      glue$glue_sql(
        "(
          {season}, {name}, {club}, {position}, {apps}, {minutes_played}, 
          {average_rating}, {player_of_the_match}, {won}, {lost}, {draw}, 
          {clean_sheets}, {conceded}, {saves_parried}, {saves_held}, 
          {saves_tipped}, {save_pct}, {penalties_faced}, {penalties_saved}, 
          {xsave_pct}, {xg_prevented}
        )",
        .con = con,
        season              = keeper$season,
        name                = keeper$name,
        club                = keeper$club,
        position            = keeper$position,
        apps                = keeper$apps,
        minutes_played      = keeper$`minutes played`,
        average_rating      = keeper$`average rating`,
        player_of_the_match = keeper$`player of the match`,
        won                 = keeper$won,
        lost                = keeper$lost,
        draw                = keeper$drawn,
        clean_sheets        = keeper$`clean sheets`,
        conceded            = keeper$conceded,
        saves_parried       = keeper$`saves parried`,
        saves_held          = keeper$`saves held`,
        saves_tipped        = keeper$`saves tipped`,
        save_pct            = keeper$`save%`,
        penalties_faced     = keeper$`penalties faced`,
        penalties_saved     = keeper$`penalties saved`,
        xsave_pct           = keeper$`xsave%`,
        xg_prevented        = keeper$`xg prevented`
      ) |> 
      glue$glue_sql_collapse(sep = ", ")
    
    safeQuery <- 
      c(insert, values, ";") |> 
      glue$glue_sql_collapse()
    
    DBI$dbExecute(con, safeQuery) |> 
      suppressWarnings()
      
    DBI$dbCommit(con)
    
  }, error = function(e) {
    DBI$dbRollback(con)
    
    # Log or handle the error
    message("Error executing query: ", e$message)
    
    stop(e$message)
  }, finally = {
    # Ensure the connection is closed
    DBI$dbDisconnect(con)
  })
}
