box::use(
  dplyr[across, mutate, if_else, where],
  lubridate,
  promises[future_promise],
  tidyr[replace_na],
)

box::use(
  app/logic/db/api[readAPI],
  app/logic/db/database[portalQuery, indexQuery],
)

#' @export
getUpdateHistory <- function(pid) {
  portalQuery(
    paste("SELECT 
            uh.time AS Time,
            mbb.username AS Username,
            uh.attribute AS `Changed attribute`,
            uh.old AS `From`,
            uh.new AS `To`
        FROM 
            updatehistory uh
        LEFT JOIN
            mybbdb.mybb_users mbb ON uh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY Time DESC")
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    )
}

#' @export
getTpeHistory <- function(pid) {
  portalQuery(
    paste("SELECT 
            tpeh.time AS Time,
            mbb.username AS Username,
            tpeh.source AS Source,
            tpeh.tpe AS `TPE Change`
        FROM 
            tpehistory tpeh
        LEFT JOIN
            mybbdb.mybb_users mbb ON tpeh.uid = mbb.uid
        WHERE 
            pid = ", pid, "
        ORDER BY time DESC")
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    )
}

#' @export
getBankHistory <- function(pid) {
  readAPI(
    "https://api.simulationsoccer.com/bank/getBankTransactions",
    query = list(pid = pid)
  )
}

#' @export
getRecentCreates <- function() {
  portalQuery(
    "SELECT pd.name, mb.username, pd.position
    FROM playerdata pd
    LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
    ORDER BY pd.created DESC
    LIMIT 10;"
  )
}

#' @export
getTopEarners <- function() {
  portalQuery(
    "SELECT 
        pd.name AS Name,
        mbb.username AS Username,
        SUM(ph.tpe) AS `TPE Earned`
    FROM 
        tpehistory ph
    JOIN 
        playerdata pd ON ph.pid = pd.pid
    LEFT JOIN
        mybbdb.mybb_users mbb ON pd.uid = mbb.uid
    WHERE 
        YEARWEEK(FROM_UNIXTIME(ph.time), 1) = YEARWEEK(CONVERT_TZ(CURTIME(), 'UTC', 'America/Los_Angeles'), 1) AND ph.source <> 'Initial TPE' AND ph.tpe > 0
    GROUP BY 
        ph.pid
    ORDER BY 
        `TPE Earned` DESC
    LIMIT 10;"
  )
}

#' @export
getPlayerNames <- function(){
  portalQuery(
    paste0(
      "SELECT name, pid, team
      FROM playerdata
      WHERE team >= -3
      ORDER BY team DESC, name;"
    )
  )
}

#' @export
getPlayers <- function(active){
  active <- if_else(active == "TRUE", 1, 0)
  
  portalQuery(
    paste0(
      "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
      pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name AS team, pd.affiliate, pd.birthplace, 
      -- Check if nationality is 3 letters and map it to the full name from portaldb.nationality, else show pd.nationality
        CASE 
            WHEN LENGTH(pd.nationality) = 3 THEN n.name
            ELSE pd.nationality 
        END AS nationality,
      pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
      pd.render, pd.`left foot`, pd.`right foot`, pd.position, pd.pos_st, pd.pos_lam, 
      pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, pd.pos_lwb, pd.pos_cdm,
      pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk, pd.acceleration, pd.agility,
      pd.balance, pd.`jumping reach`, pd.`natural fitness`, pd.pace, pd.stamina, pd.strength, 
      pd.corners, pd.crossing, pd.dribbling, pd.finishing, pd.`first touch`, pd.`free kick`, 
      pd.heading, pd.`long shots`, pd.`long throws`, pd.marking, pd.passing, pd.`penalty taking`, 
      pd.tackling, pd.technique, pd.aggression, pd.anticipation, pd.bravery, pd.composure, 
      pd.concentration, pd.decisions, pd.determination, pd.flair, pd.leadership, pd.`off the ball`, 
      pd.positioning, pd.teamwork, pd.vision, pd.`work rate`, pd.`aerial reach`, pd.`command of area`, 
      pd.communication, pd.eccentricity, pd.handling, pd.kicking, pd.`one on ones`, pd.reflexes, 
      pd.`tendency to rush`, pd.`tendency to punch`, pd.throwing, pd.traits, pd.rerollused, pd.redistused,
      mb.username, mbuf.fid4 AS discord, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
        CASE 
            WHEN pd.tpe <= 350 THEN 1000000
            WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
            WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
            WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
            WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
            WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
            WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
            WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
            WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
            WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
            WHEN pd.tpe > 1700 THEN 6000000
            ELSE NULL
        END AS `minimum salary`,
      SUM(CASE WHEN bt.status = 1 THEN bt.transaction ELSE 0 END) AS bankBalance,
      n.region,
      o.name AS organization
      FROM playerdata pd
      LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
      LEFT JOIN useractivity ua ON pd.uid = ua.uid
      LEFT JOIN userstatuses us ON ua.status_u = us.status
      LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
      LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
      LEFT JOIN mybbdb.mybb_userfields mbuf ON pd.uid = mbuf.ufid
      LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation OR pd.nationality = n.name
      LEFT JOIN banktransactions bt ON pd.pid = bt.pid
      LEFT JOIN organizations o ON pd.team = o.id
      WHERE pd.status_p >= ", active , "
      GROUP BY pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
      pd.created, pd.tpe, pd.tpeused, pd.tpebank, t.name, pd.affiliate, pd.birthplace, 
      n.name, pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
      pd.render, pd.`left foot`, pd.`right foot`, pd.position, pd.pos_st, pd.pos_lam, 
      pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, pd.pos_lwb, pd.pos_cdm,
      pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk, pd.acceleration, pd.agility,
      pd.balance, pd.`jumping reach`, pd.`natural fitness`, pd.pace, pd.stamina, pd.strength, 
      pd.corners, pd.crossing, pd.dribbling, pd.finishing, pd.`first touch`, pd.`free kick`, 
      pd.heading, pd.`long shots`, pd.`long throws`, pd.marking, pd.passing, pd.`penalty taking`, 
      pd.tackling, pd.technique, pd.aggression, pd.anticipation, pd.bravery, pd.composure, 
      pd.concentration, pd.decisions, pd.determination, pd.flair, pd.leadership, pd.`off the ball`, 
      pd.positioning, pd.teamwork, pd.vision, pd.`work rate`, pd.`aerial reach`, pd.`command of area`, 
      pd.communication, pd.eccentricity, pd.handling, pd.kicking, pd.`one on ones`, pd.reflexes, 
      pd.`tendency to rush`, pd.`tendency to punch`, pd.throwing, pd.traits, pd.rerollused, pd.redistused,
      mb.username, mbuf.fid4, us.desc, ps.desc, n.region
      ORDER BY pd.created DESC;"
    )
  ) |>
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    ) |> 
    suppressWarnings()
}

#' @export
getDraftClass <- function(class = NULL) {
  # If no class is given it defaults to the youngest
  if(class |> is.null()){
    class <- indexQuery("SELECT season FROM seasoninfo ORDER BY season DESC LIMIT 1;") |> unlist() + 1
  }
  
  portalQuery(
    paste(
      "SELECT pd.name, pd.pid, pd.tpe, t.name AS team, mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, pd.position, sum(bt.transaction) AS bankBalance 
        FROM playerdata pd
        LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
        LEFT JOIN useractivity ua ON pd.uid = ua.uid
        LEFT JOIN userstatuses us ON ua.status_u = us.status
        LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
        LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
        LEFT JOIN banktransactions bt ON pd.pid = bt.pid
      WHERE pd.class = ", paste0("'S", class, "'"), " AND pd.status_p > 0 AND bt.status = 1
      GROUP BY pd.name, pd.pid, pd.tpe, t.name, mb.username, us.desc, ps.desc, pd.position
      ORDER BY pd.tpe DESC;"
    )
  ) |>
    suppressWarnings()
}

#' @export
getStandings <- function(league, season){
  indexQuery(
    paste(
      "SELECT
    Team,
    COUNT(*) AS MatchesPlayed,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 1
        ELSE 0
    END) AS Wins,
    SUM(CASE
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Draws,
    SUM(CASE
        WHEN (Home = Team AND HomeScore < AwayScore) OR (Away = Team AND AwayScore < HomeScore) THEN 1
        ELSE 0
    END) AS Losses,
    SUM(CASE
        WHEN (Home = Team) THEN HomeScore
        ELSE AwayScore
    END) AS GoalsFor,
    SUM(CASE
        WHEN (Home = Team) THEN AwayScore
        ELSE HomeScore
    END) AS GoalsAgainst,
    SUM(CASE
        WHEN (Home = Team AND HomeScore > AwayScore) OR (Away = Team AND AwayScore > HomeScore) THEN 3
        WHEN (HomeScore = AwayScore) THEN 1
        ELSE 0
    END) AS Points,
    SUM(CASE
        WHEN (Home = Team) THEN HomeScore
        ELSE AwayScore
    END) - SUM(CASE
        WHEN (Home = Team) THEN AwayScore
        ELSE HomeScore
    END) AS GoalDifference
FROM (
    SELECT Home AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE HomeScore IS NOT NULL AND matchtype ", 
      if_else(
        league == "ALL", 
        '>= 0', 
        paste0("=", league)), 
      if_else(
        season == "ALL", 
        "", 
        paste0(" AND Season = ", season)
      ),  
      "UNION ALL
    SELECT Away AS Team, Home, Away, HomeScore, AwayScore FROM schedule WHERE HomeScore IS NOT NULL AND matchtype ", if_else(league == "ALL", '>= 0', paste0("=", league)), if_else(season == "ALL", "", paste0(" AND Season = ", season)),
      ") AS combined
GROUP BY Team
ORDER BY Points DESC, GoalDifference DESC, GoalsFor DESC;"
    )
  ) |>
    suppressWarnings()
}

#' @export
getSchedule <- function(league, season){
  indexQuery(
    paste(
      "SELECT IRLDate, MatchType, MatchDay, Home, Away, HomeScore, AwayScore, ExtraTime, Penalties
      FROM schedule
      WHERE season ",
      if_else(season == "ALL", "> 0", paste0("=", season)), 
      "AND MatchType",
      if_else(league == "ALL", '< 10', paste0("=", league)),
      "ORDER BY IRLDate;"
    )
  )
}

#' @export
getPlayer <- function(pid){
  portalQuery(
    paste(
      "SELECT pd.uid, pd.pid, pd.status_p, pd.first, pd.last, pd.name, pd.class, 
        pd.created, pd.tpe, pd.tpeused, pd.tpebank, pd.team AS organization, t.name AS team, pd.affiliate, pd.birthplace, 
        -- Check if nationality is 3 letters and map it to the full name from portaldb.nationality, else show pd.nationality
        CASE 
            WHEN LENGTH(pd.nationality) = 3 THEN n.name
            ELSE pd.nationality 
        END AS nationality,
        pd.height, pd.weight, pd.hair_color, pd.hair_length, pd.skintone, 
        pd.render, pd.`left foot`, pd.`right foot`, pd.position, pd.pos_st, pd.pos_lam, 
        pd.pos_cam, pd.pos_ram, pd.pos_lm, pd.pos_cm, pd.pos_rm, pd.pos_lwb, pd.pos_cdm,
        pd.pos_rwb, pd.pos_ld, pd.pos_cd, pd.pos_rd, pd.pos_gk, pd.acceleration, pd.agility,
        pd.balance, pd.`jumping reach`, pd.`natural fitness`, pd.pace, pd.stamina, pd.strength, 
        pd.corners, pd.crossing, pd.dribbling, pd.finishing, pd.`first touch`, pd.`free kick`, 
        pd.heading, pd.`long shots`, pd.`long throws`, pd.marking, pd.passing, pd.`penalty taking`, 
        pd.tackling, pd.technique, pd.aggression, pd.anticipation, pd.bravery, pd.composure, 
        pd.concentration, pd.decisions, pd.determination, pd.flair, pd.leadership, pd.`off the ball`, 
        pd.positioning, pd.teamwork, pd.vision, pd.`work rate`, pd.`aerial reach`, pd.`command of area`, 
        pd.communication, pd.eccentricity, pd.handling, pd.kicking, pd.`one on ones`, pd.reflexes, 
        pd.`tendency to rush`, pd.`tendency to punch`, pd.throwing, pd.traits, pd.rerollused, pd.redistused,
        mb.username, us.desc AS `userStatus`, ps.desc AS `playerStatus`, 
              CASE 
                WHEN pd.tpe <= 350 THEN 1000000
                WHEN pd.tpe BETWEEN 351 AND 500 THEN 1500000
                WHEN pd.tpe BETWEEN 501 AND 650 THEN 2000000
                WHEN pd.tpe BETWEEN 651 AND 800 THEN 2500000
                WHEN pd.tpe BETWEEN 801 AND 950 THEN 3000000
                WHEN pd.tpe BETWEEN 951 AND 1100 THEN 3500000
                WHEN pd.tpe BETWEEN 1101 AND 1250 THEN 4000000
                WHEN pd.tpe BETWEEN 1251 AND 1400 THEN 4500000
                WHEN pd.tpe BETWEEN 1401 AND 1550 THEN 5000000
                WHEN pd.tpe BETWEEN 1551 AND 1700 THEN 5500000
                WHEN pd.tpe > 1700 THEN 6000000
                ELSE NULL
            END AS `minimum salary`
          FROM playerdata pd
          LEFT JOIN mybbdb.mybb_users mb ON pd.uid = mb.uid
          LEFT JOIN useractivity ua ON pd.uid = ua.uid
          LEFT JOIN userstatuses us ON ua.status_u = us.status
          LEFT JOIN playerstatuses ps ON pd.status_p = ps.status
          LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
          LEFT JOIN portaldb.nationality n ON pd.nationality = n.abbreviation",
      paste("WHERE pd.pid = ", pid, ";")
    )
  ) |>
    mutate(
      across(where(is.numeric), ~replace_na(.x, 5))
    )
}

#' @export
getOrganizations <- function(){
  portalQuery(
    "SELECT o.ID, o.name AS organization, o.abbr AS abbreviation, t.name, t.primaryColor, t.secondaryColor, t.city 
    FROM teams AS t 
    LEFT JOIN organizations AS o ON t.orgID = o.ID 
    ORDER BY o.ID"
  )
}

#' @export
getAcademyIndex <- function(outfield = TRUE, season){
  if(outfield){
    indexQuery(
      paste(
        "SELECT
            `name`, `club`, `position`, `apps`, `minutes played`,`player of the match`,`distance run (km)`, 
            `goals`,`assists`,`xg`,`shots on target`,`shots`,`penalties taken`,`penalties scored`,`successfull passes` AS `successful passes`,
            `attempted passes`,`successfull passes` / `attempted passes` * 100 AS `pass%`,`key passes`,  
            `successful crosses`,`attempted crosses`,`successful crosses` / `attempted crosses` * 100 AS `cross%`,
            `chances created`,`successful headers`,`attempted headers`,`successful headers` / `attempted headers` * 100 AS `header%`,
            `key headers`,`dribbles`,`tackles won`,`attempted tackles`,`tackles won` / `attempted tackles` * 100 AS `tackle%`,
            `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,`yellow cards`,`red cards`,
            `fouls`,`fouls against`,`offsides`,`xa`,`xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
            `successful open play crosses`,`attempted open play crosses`,`shots blocked`,`progressive passes`,
            `successful presses`,`attempted presses`,`goals outside box`,`average rating`,
            CASE 
                WHEN IFNULL(`attempted presses`, 0) = 0 THEN 0
                ELSE (`successful presses` / `attempted presses`) * 100
        	  END AS `press%`,
              CASE 
                WHEN IFNULL(`attempted open play crosses`, 0) = 0 THEN 0
                ELSE (`successful open play crosses` / `attempted open play crosses`) * 100
            END AS `open play crosses%`,
            `shots on target` / `shots` * 100 AS `shot accuracy%`,
            `xG` - 0.83*`penalties taken` AS `pen adj xG`
        FROM academyoutfield WHERE season = ", season, ";",
              sep = ""
      )
    ) |> 
      suppressWarnings()
  } else {
    indexQuery(
      paste(
        "SELECT
            `name`, `club`, `apps`, `minutes played`, `average rating`, `player of the match`, won, lost, draw, `clean sheets`, conceded, `saves parried`, `saves held`, 
            `saves tipped`, (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
            `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`
        FROM academykeeper WHERE season = ", season, ";",
              sep = ""
      )
    ) |> 
      suppressWarnings()
  }
}

#' @export
getLeagueIndex <- function(outfield = TRUE, season, league = "ALL"){
  if(outfield){
    indexQuery(
      paste(
        "SELECT
            `name`, `club`, `position`, `apps`, `minutes played`,`player of the match`,`distance run (km)`, 
            `goals`,`assists`,`xg`,`shots on target`,`shots`,`penalties taken`,`penalties scored`,`successful passes`,
            `attempted passes`,`successful passes` / `attempted passes` * 100 AS `pass%`,`key passes`,  
            `successful crosses`,`attempted crosses`,`successful crosses` / `attempted crosses` * 100 AS `cross%`,
            `chances created`,`successful headers`,`attempted headers`,`successful headers` / `attempted headers` * 100 AS `header%`,
            `key headers`,`dribbles`,`tackles won`,`attempted tackles`,`tackles won` / `attempted tackles` * 100 AS `tackle%`,
            `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,`yellow cards`,`red cards`,
            `fouls`,`fouls against`,`offsides`,`xa`,`xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
            `successful open play crosses`,`attempted open play crosses`,`shots blocked`,`progressive passes`,
            `successful presses`,`attempted presses`,`goals outside box`,`average rating`,
            CASE 
                WHEN IFNULL(`attempted presses`, 0) = 0 THEN 0
                ELSE (`successful presses` / `attempted presses`) * 100
        	  END AS `press%`,
              CASE 
                WHEN IFNULL(`attempted open play crosses`, 0) = 0 THEN 0
                ELSE (`successful open play crosses` / `attempted open play crosses`) * 100
            END AS `open play crosses%`,
            `shots on target` / `shots` * 100 AS `shot accuracy%`,
            `xG` - 0.83*`penalties taken` AS `pen adj xG`
        FROM (
          SELECT
            `name`,
            GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
            MAX(`position`) AS `position`,
            SUM(`apps`) AS `apps`,
            SUM(`minutes played`) AS `minutes played`,
            SUM(`distance run (km)`) AS `distance run (km)`,
            SUM(`goals`) AS `goals`,
            SUM(`assists`) AS `assists`,
            SUM(`xg`) AS `xg`,
            SUM(`shots on target`) AS `shots on target`,
            SUM(`shots`) AS `shots`,
            SUM(`penalties taken`) AS `penalties taken`,
            SUM(`penalties scored`) AS `penalties scored`,
            SUM(`successful passes`) AS `successful passes`,
            SUM(`attempted passes`) AS `attempted passes`,
            SUM(`pass%`) AS `pass%`,
            SUM(`key passes`) AS `key passes`,
            SUM(`successful crosses`) AS `successful crosses`,
            SUM(`attempted crosses`) AS `attempted crosses`,
            SUM(`cross%`) AS `cross%`,
            SUM(`chances created`) AS `chances created`,
            SUM(`successful headers`) AS `successful headers`,
            SUM(`attempted headers`) AS `attempted headers`,
            SUM(`header%`) AS `header%`,
            SUM(`key headers`) AS `key headers`,
            SUM(`dribbles`) AS `dribbles`,
            SUM(`tackles won`) AS `tackles won`,
            SUM(`attempted tackles`) AS `attempted tackles`,
            SUM(`tackle%`) AS `tackle%`,
            SUM(`key tackles`) AS `key tackles`,
            SUM(`interceptions`) AS `interceptions`,
            SUM(`clearances`) AS `clearances`,
            SUM(`mistakes leading to goals`) AS `mistakes leading to goals`,
            SUM(`yellow cards`) AS `yellow cards`,
            SUM(`red cards`) AS `red cards`,
            SUM(`fouls`) AS `fouls`,
            SUM(`fouls against`) AS `fouls against`,
            SUM(`offsides`) AS `offsides`,
            SUM(`xa`) AS `xa`,
            SUM(`xg overperformance`) AS `xg overperformance`,
            SUM(`fk shots`) AS `fk shots`,
            SUM(`blocks`) AS `blocks`,
            SUM(`open play key passes`) AS `open play key passes`,
            SUM(`successful open play crosses`) AS `successful open play crosses`,
            SUM(`attempted open play crosses`) AS `attempted open play crosses`,
            SUM(`shots blocked`) AS `shots blocked`,
            SUM(`progressive passes`) AS `progressive passes`,
            SUM(`successful presses`) AS `successful presses`,
            SUM(`attempted presses`) AS `attempted presses`,
            SUM(`goals outside box`) AS `goals outside box`,
            SUM(`player of the match`) AS `player of the match`,
            AVG(`average rating`) AS `average rating`
          FROM (
            SELECT
              `name`,`club`,`position`,`apps`,`minutes played`,`distance run (km)`,`average rating`,`player of the match`,
              `goals`,`assists`,`xg`,`shots on target`,`shots`,`penalties taken`,`penalties scored`,`successful passes`,
              `attempted passes`,`pass%`,`key passes`,`successful crosses`,`attempted crosses`,`cross%`,`chances created`,
              `successful headers`,`attempted headers`,`header%`,`key headers`,`dribbles`,`tackles won`,`attempted tackles`,
              `tackle%`,`key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,`yellow cards`,`red cards`,
              `fouls`,`fouls against`,`offsides`,`xa`,`xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
              `successful open play crosses`,`attempted open play crosses`,`shots blocked`,`progressive passes`,
              `successful presses`,`attempted presses`,`goals outside box`
            FROM `gamedataoutfield` AS gd
            JOIN schedule AS s ON gd.gid = s.gid
            ",if_else(league == "ALL" & season == "ALL",
                      "",
                      if_else(league == "ALL",
                              paste("WHERE s.season = ", season, sep = ""),
                              if_else(season == "ALL",
                                      paste("WHERE s.Matchtype = ", league, sep = ""),
                                      paste("WHERE s.Matchtype = '", league, "' AND s.season = ", season, sep = ""))
                      )
            ),
            ") `q01`
            GROUP BY `name`
          ) `q01`",
        sep = ""
      )
    ) |> 
      suppressWarnings()
  } else {
    indexQuery(
      paste(
        "SELECT
            `name`, `club`, `apps`, `minutes played`, `average rating`, `player of the match`, SUM(won) AS won, 
            SUM(lost) AS lost, SUM(drawn) AS drawn,  `clean sheets`, conceded, `saves parried`, `saves held`, 
            `saves tipped`, (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
            `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`
        FROM (
          SELECT
            `name`,
            GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
            SUM(`apps`) AS `apps`,
            SUM(`minutes played`) AS `minutes played`,
            AVG(`average rating`) AS `average rating`,
            SUM(`player of the match`) AS `player of the match`,
            SUM(CASE 
                  WHEN (Home = club AND HomeScore > AwayScore) OR 
                       (Away = club AND AwayScore > HomeScore)
                  THEN 1 ELSE 0 END) AS `won`,
              SUM(CASE 
                  WHEN (Home = club AND HomeScore < AwayScore) OR 
                       (Away = club AND AwayScore < HomeScore)
                  THEN 1 ELSE 0 END) AS `lost`,
              SUM(CASE 
                  WHEN HomeScore = AwayScore THEN 1 ELSE 0 END) AS `drawn`,
            SUM(`clean sheets`) AS `clean sheets`,
            SUM(`conceded`) AS `conceded`,
            SUM(`saves parried`) AS `saves parried`,
            SUM(`saves held`) AS `saves held`,
            SUM(`saves tipped`) AS `saves tipped`,
            SUM(`save%`) AS `save%`,
            SUM(`penalties faced`) AS `penalties faced`,
            SUM(`penalties saved`) AS `penalties saved`,
            AVG(`xsave%`) AS `xsave%`,
            SUM(`xg prevented`) AS `xg prevented`    
          FROM (
            SELECT
              `name`,
              `club`,
              `apps`,
              `minutes played`,
              `average rating`,
              `player of the match`,
              `clean sheets`,
              `conceded`,
              `saves parried`,
              `saves held`,  
              `saves tipped`,  
              `save%`,  
              `penalties faced`,  
              `penalties saved`,
              `xsave%`, 
              `xg prevented`,
              s.*
            FROM `gamedatakeeper`AS gd
            JOIN schedule AS s ON gd.gid = s.gid
            ",if_else(league == "ALL" & season == "ALL",
                      "",
                      if_else(league == "ALL",
                              paste("WHERE s.season = ", season, sep = ""),
                              if_else(season == "ALL",
                                      paste("WHERE s.Matchtype = ", league, sep = ""),
                                      paste("WHERE s.Matchtype = '", league, "' AND s.season = ", season, sep = ""))
                      )
            ),
            ") `q01`
            GROUP BY `name`
          ) `q02`
          GROUP BY `name`",
      sep = "")
    ) |> 
      suppressWarnings()
  }
}