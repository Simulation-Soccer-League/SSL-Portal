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
  purrr[pmap],
  rvest[read_html, html_elements, html_table],
  stats[setNames],
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
  ],
  
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
          Club == "Rapid Magyar" ~ "Rapid Magyar SC",
          Club == "Seoul Mythic FC" ~ "Seoul MFC",
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
importGameData <- function(data) {
  outfield <- data$o
  keeper <- data$k
  
  for (i in seq_len(nrow(outfield))) {
    indexQuery(
      query = "
      INSERT INTO gamedataoutfield (
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
      VALUES (
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
      );",
      name                 = outfield$name[i],
      club                 = outfield$club[i],
      position             = outfield$position[i],
      acc                  = outfield$acc[i],
      aer                  = outfield$aer[i],
      agg                  = outfield$agg[i],
      agi                  = outfield$agi[i],
      ant                  = outfield$ant[i],
      bal                  = outfield$bal[i],
      bra                  = outfield$bra[i],
      cmd                  = outfield$cmd[i],
      com                  = outfield$com[i],
      cmp                  = outfield$cmp[i],
      cnt                  = outfield$cnt[i],
      cor                  = outfield$cor[i],
      cro                  = outfield$cro[i],
      dec                  = outfield$dec[i],
      det                  = outfield$det[i],
      dri                  = outfield$dri[i],
      ecc                  = outfield$ecc[i],
      fin                  = outfield$fin[i],
      fir                  = outfield$fir[i],
      fla                  = outfield$fla[i],
      fre                  = outfield$fre[i],
      han                  = outfield$han[i],
      hea                  = outfield$hea[i],
      jum                  = outfield$jum[i],
      kic                  = outfield$kic[i],
      ldr                  = outfield$ldr[i],
      lon                  = outfield$lon[i],
      l_th                 = outfield$`l th`[i],
      mar                  = outfield$mar[i],
      nat                  = outfield$nat[i],
      otb                  = outfield$otb[i],
      one_v_one            = outfield$`1v1`[i],
      pac                  = outfield$pac[i],
      pas                  = outfield$pas[i],
      pen                  = outfield$pen[i],
      pos                  = outfield$pos[i],
      pun                  = outfield$pun[i],
      ref                  = outfield$ref[i],
      tro                  = outfield$tro[i],
      sta                  = outfield$sta[i],
      str                  = outfield$str[i],
      tck                  = outfield$tck[i],
      tea                  = outfield$tea[i],
      tec                  = outfield$tec[i],
      thr                  = outfield$thr[i],
      vis                  = outfield$vis[i],
      wor                  = outfield$wor[i],
      apps                 = outfield$apps[i],
      minutes_played       = outfield$`minutes played`[i],
      distance_run_km      = outfield$`distance run (km)`[i],
      average_rating       = outfield$`average rating`[i],
      player_of_the_match  = outfield$`player of the match`[i],
      goals                = outfield$goals[i],
      assists              = outfield$assists[i],
      xg                   = outfield$xg[i],
      xa                   = outfield$xa[i],
      shots_on_target      = outfield$`shots on target`[i],
      shots                = outfield$shots[i],
      penalties_taken      = outfield$`penalties taken`[i],
      penalties_scored     = outfield$`penalties scored`[i],
      successful_passes    = outfield$`successful passes`[i],
      attempted_passes     = outfield$`attempted passes`[i],
      pass_pct             = outfield$`pass%`[i],
      key_passes           = outfield$`key passes`[i],
      successful_crosses   = outfield$`successful crosses`[i],
      attempted_crosses    = outfield$`attempted crosses`[i],
      cross_pct            = outfield$`cross%`[i],
      chances_created      = outfield$`chances created`[i],
      successful_headers   = outfield$`successful headers`[i],
      attempted_headers    = outfield$`attempted headers`[i],
      header_pct           = outfield$`header%`[i],
      key_headers          = outfield$`key headers`[i],
      dribbles             = outfield$dribbles[i],
      tackles_won          = outfield$`tackles won`[i],
      attempted_tackles    = outfield$`attempted tackles`[i],
      tackle_pct           = outfield$`tackle%`[i],
      key_tackles          = outfield$`key tackles`[i],
      interceptions        = outfield$interceptions[i],
      clearances           = outfield$clearances[i],
      mistakes_leading_to_goals = outfield$`mistakes leading to goals`[i],
      yellow_cards         = outfield$`yellow cards`[i],
      red_cards            = outfield$`red cards`[i],
      fouls                = outfield$fouls[i],
      fouls_against        = outfield$`fouls against`[i],
      offsides             = outfield$offsides[i],
      xg_overperformance   = outfield$`xg overperformance`[i],
      goals_outside_box    = outfield$`goals outside box`[i],
      fk_shots             = outfield$`fk shots`[i],
      blocks               = outfield$blocks[i],
      open_play_key_passes = outfield$`open play key passes`[i],
      successful_open_play_crosses = outfield$`successful open play crosses`[i],
      attempted_open_play_crosses  = outfield$`attempted open play crosses`[i],
      shots_blocked            = outfield$`shots blocked`[i],
      progressive_passes       = outfield$`progressive passes`[i],
      successful_presses       = outfield$`successful presses`[i],
      attempted_presses        = outfield$`attempted presses`[i],
      gid                      = outfield$gid[i],
      type                     = "set"
    )
  }
  
  for (i in seq_len(nrow(keeper))) {
    indexQuery(
      query = "
      INSERT INTO gamedatakeeper (
        name, club, apps, `minutes played`, 
        `average rating`, `player of the match`, 
        `clean sheets`, conceded, `saves parried`, `saves held`, 
        `saves tipped`, `save%`, `penalties faced`, `penalties saved`, 
        `xsave%`, `xg prevented`, gid
      )
      VALUES (
        {name}, {club}, {apps}, {minutes_played}, 
        {average_rating}, {player_of_the_match}, 
        {clean_sheets}, {conceded}, {saves_parried}, {saves_held}, 
        {saves_tipped}, {save_pct}, {penalties_faced}, {penalties_saved}, 
        {xsave_pct}, {xg_prevented}, {gid}
      );",
      name                = keeper$name[i],
      club                = keeper$club[i],
      apps                = keeper$apps[i],
      minutes_played      = keeper$`minutes played`[i],
      average_rating      = keeper$`average rating`[i],
      player_of_the_match = keeper$`player of the match`[i],
      clean_sheets        = keeper$`clean sheets`[i],
      conceded            = keeper$conceded[i],
      saves_parried       = keeper$`saves parried`[i],
      saves_held          = keeper$`saves held`[i],
      saves_tipped        = keeper$`saves tipped`[i],
      save_pct            = keeper$`save%`[i],
      penalties_faced     = keeper$`penalties faced`[i],
      penalties_saved     = keeper$`penalties saved`[i],
      xsave_pct           = keeper$`xsave%`[i],
      xg_prevented        = keeper$`xg prevented`[i],
      gid                 = keeper$gid[i],
      type                = "set"
    )
  }
  
}

#' @export
importAcademyData <- function(data) {
  outfield <- data$o
  keeper <- data$k
  
  for (i in seq_len(nrow(outfield))) {
    indexQuery(
      query = "
    INSERT INTO academyoutfield (
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
    VALUES (
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
    );",
      season                   = outfield$season[i],
      name                     = outfield$name[i],
      club                     = outfield$club[i],
      position                 = outfield$position[i],
      apps                     = outfield$apps[i],
      minutes_played           = outfield$`minutes played`[i],
      distance_run_km          = outfield$`distance run (km)`[i],
      average_rating           = outfield$`average rating`[i],
      player_of_the_match      = outfield$`player of the match`[i],
      goals                    = outfield$goals[i],
      assists                  = outfield$assists[i],
      xg                       = outfield$xg[i],
      xa                       = outfield$xa[i],
      xg_overperformance       = outfield$`xg overperformance`[i],
      goals_outside_box        = outfield$`goals outside box`[i],
      shots_on_target          = outfield$`shots on target`[i],
      shots                    = outfield$shots[i],
      fk_shots                 = outfield$`fk shots`[i],
      blocks                   = outfield$blocks[i],
      penalties_taken          = outfield$`penalties taken`[i],
      penalties_scored         = outfield$`penalties scored`[i],
      successful_passes        = outfield$`successful passes`[i],
      attempted_passes         = outfield$`attempted passes`[i],
      pass_pct                 = outfield$`pass%`[i],
      key_passes               = outfield$`key passes`[i],
      open_play_key_passes     = outfield$`open play key passes`[i],
      successful_open_play_crosses = outfield$`successful open play crosses`[i],
      attempted_open_play_crosses  = outfield$`attempted open play crosses`[i],
      successful_crosses       = outfield$`successful crosses`[i],
      attempted_crosses        = outfield$`attempted crosses`[i],
      cross_pct                = outfield$`cross%`[i],
      chances_created          = outfield$`chances created`[i],
      successful_headers       = outfield$`successful headers`[i],
      attempted_headers        = outfield$`attempted headers`[i],
      header_pct               = outfield$`header%`[i],
      key_headers              = outfield$`key headers`[i],
      dribbles                 = outfield$dribbles[i],
      tackles_won              = outfield$`tackles won`[i],
      attempted_tackles        = outfield$`attempted tackles`[i],
      tackle_pct               = outfield$`tackle%`[i],
      key_tackles              = outfield$`key tackles`[i],
      interceptions            = outfield$interceptions[i],
      shots_blocked            = outfield$`shots blocked`[i],
      clearances               = outfield$clearances[i],
      mistakes_leading_to_goals = outfield$`mistakes leading to goals`[i],
      yellow_cards             = outfield$`yellow cards`[i],
      red_cards                = outfield$`red cards`[i],
      fouls                    = outfield$fouls[i],
      fouls_against            = outfield$`fouls against`[i],
      offsides                 = outfield$offsides[i],
      progressive_passes       = outfield$`progressive passes`[i],
      successful_presses       = outfield$`successful presses`[i],
      attempted_presses        = outfield$`attempted presses`[i],
      type                     = "set"
    )
  }
 
  for (i in seq_len(nrow(keeper))) {
    indexQuery(
      query = "
    INSERT INTO academykeeper (
      season, name, club, position, apps, `minutes played`, 
      `average rating`, `player of the match`, won, lost, draw, 
      `clean sheets`, conceded, `saves parried`, `saves held`, 
      `saves tipped`, `save%`, `penalties faced`, `penalties saved`, 
      `xsave%`, `xg prevented`
    )
    VALUES (
      {season}, {name}, {club}, {position}, {apps}, {minutes_played}, 
      {average_rating}, {player_of_the_match}, {won}, {lost}, {draw}, 
      {clean_sheets}, {conceded}, {saves_parried}, {saves_held}, 
      {saves_tipped}, {save_pct}, {penalties_faced}, {penalties_saved}, 
      {xsave_pct}, {xg_prevented}
    );",
      season              = keeper$season[i],
      name                = keeper$name[i],
      club                = keeper$club[i],
      position            = keeper$position[i],
      apps                = keeper$apps[i],
      minutes_played      = keeper$`minutes played`[i],
      average_rating      = keeper$`average rating`[i],
      player_of_the_match = keeper$`player of the match`[i],
      won                 = keeper$won[i],
      lost                = keeper$lost[i],
      draw                = keeper$drawn[i],
      clean_sheets        = keeper$`clean sheets`[i],
      conceded            = keeper$conceded[i],
      saves_parried       = keeper$`saves parried`[i],
      saves_held          = keeper$`saves held`[i],
      saves_tipped        = keeper$`saves tipped`[i],
      save_pct            = keeper$`save%`[i],
      penalties_faced     = keeper$`penalties faced`[i],
      penalties_saved     = keeper$`penalties saved`[i],
      xsave_pct           = keeper$`xsave%`[i],
      xg_prevented        = keeper$`xg prevented`[i],
      type                = "set"
    )
  }
}
