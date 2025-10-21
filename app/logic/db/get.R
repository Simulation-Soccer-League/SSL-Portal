# nolint: line_length_linter
box::use(
  dplyr[across, if_else, mutate, where],
  lubridate,
  tidyr[replace_na],
)

box::use(
  app / logic / db / database[indexQuery, portalQuery],
)

#' @export
getUpdateHistory <- function(pid) {
  portalQuery(
    "SELECT Time, Username, `Changed attribute`, `From`, `To`
    FROM updatehistoryview
    WHERE pid = {pid}
    ORDER BY Time DESC;",
    pid = pid
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
    "SELECT Time, Username, Source, `TPE Change`
    FROM tpehistoryview
    WHERE pid = {pid}
    ORDER BY Time DESC;",
    pid = pid
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    )
}

#' @export
getBankHistory <- function(pid) {
  portalQuery(
    "SELECT Time, Player, Username, Source, Transaction, Status
    FROM bankhistoryview
    WHERE pid = {pid}
    ORDER BY Time DESC;",
    pid = pid
  ) |>
    mutate(
      Time = Time |>
        as.numeric() |>
        lubridate$as_datetime(tz = "US/Pacific")
    )
}

#' @export
getBankTransactions <- function(status) {
  portalQuery(
    "SELECT *
    FROM bankhistoryview
    WHERE status IN ({status*});",
    status = status
  )
}


#' @export
getRecentCreates <- function() {
  portalQuery(
    "SELECT name, username, position, pid
    FROM allplayersview
    ORDER BY created DESC
    LIMIT 10;"
  )
}

#' @export
getTopEarners <- function() {
  portalQuery(
    query = "
      WITH t AS (
        SELECT
          pid,
          SUM(`TPE Change`)    AS `TPE Earned`
        FROM
          tpehistoryview
        WHERE
          YEARWEEK(
            FROM_UNIXTIME(time), 1
          ) = YEARWEEK(
            CONVERT_TZ(CURTIME(), 'UTC', 'America/Los_Angeles'), 1
          )
          AND source NOT IN ('Initial TPE', 'Regression')
        GROUP BY
          pid
      )
      SELECT
        pd.name,
        pd.username,
        t.`TPE Earned`,
        pd.pid
      FROM t
      JOIN allplayersview pd
        ON pd.pid = t.pid
      ORDER BY
        t.`TPE Earned` DESC
      LIMIT 10;
    "
  )
}

#' @export
getPlayerNames <- function(active = FALSE) {
  if (active) {
    portalQuery(
      "SELECT name, pid, team
      FROM allplayersview
      WHERE team <> 'Retired'
      ORDER BY created;"
    )
  } else {
    portalQuery(
      "SELECT name, pid, team
      FROM allplayersview
      ORDER BY created;"
    )
  }
  
}

#' @export
getActivePlayer <- function(uid) {
  portalQuery(
    "SELECT pid
    FROM allplayersview
    WHERE status_p = 1 AND uid = {uid};",
    uid = uid
  ) |> 
    unlist()
}

#' @export
getPlayers <- function(active) {
  active <- if_else(active == "TRUE", 1, 0)

  portalQuery(
    "SELECT *
    FROM allplayersview
    WHERE status_p >= {active}",
    active = active
  ) |>
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 5))
    ) |>
    suppressWarnings()
}


#' @export
getPlayersFromOrganization <- function(uid) {
  portalQuery(
    "SELECT *
    FROM allplayersview
    WHERE organization = (
      SELECT organization
      FROM allplayersview
      WHERE uid = {uid} AND organization <> 'Academy'
    );",
    uid = uid
  ) |>
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 5))
    ) |>
    suppressWarnings()
}

#' @export
getChangedBuilds <- function() {
  ## Gets date of the start of the week in Pacific
  weekEnd <- 
    lubridate$now() |> 
    lubridate$with_tz("US/Pacific") |> 
    lubridate$floor_date("week", week_start = "Monday") |> 
    as.numeric() |> 
    sum(c(-1))
  
  weekStart <- 
    lubridate$now() |> 
    lubridate$with_tz("US/Pacific") |> 
    lubridate$floor_date("week", week_start = "Monday") |> 
    as.numeric() |> 
    sum(c(-604800))
  
  
  portalQuery(
    query = 
      "SELECT t.name AS teamName, wb.*, uh.attribute as Attribute, uh.old, uh.new
        FROM playerdata pd
        LEFT JOIN weeklybuilds wb ON pd.pid = wb.pid
        JOIN updatehistory uh ON pd.pid = uh.pid
        LEFT JOIN teams t ON pd.team = t.orgID AND pd.affiliate = t.affiliate
        WHERE uh.Time < {weekEnd} AND uh.Time > {weekStart} AND uh.uid <> 1;",
    weekEnd = weekEnd,
    weekStart = weekStart
  )
}

#' @export
getDraftClass <- function(class = NULL) {
  # If no class is given it defaults to the youngest
  if (class |> is.null()) {
    class <- 
      indexQuery(
        "SELECT season 
        FROM seasoninfo 
        ORDER BY season DESC 
        LIMIT 1;") |> 
      unlist() + 1
  }

  portalQuery(
    "SELECT
      name, pid, tpe, team, username, userStatus, playerStatus, position, bankBalance
    FROM 
      allplayersview
    WHERE
      class = {class} AND 
      status_p  >= 0
    ORDER BY tpe DESC",
    class = paste0("S", class)
  ) |>
    suppressWarnings()
}

#' @export
getStandings <- function(league, season) {
  indexQuery(
    query = "
      SELECT
        Team,
        COUNT(*) AS MatchesPlayed,
        SUM(CASE
              WHEN (Home = Team AND HomeScore > AwayScore)
                OR (Away = Team AND AwayScore > HomeScore)
              THEN 1 ELSE 0
            END) AS Wins,
        SUM(CASE
              WHEN HomeScore = AwayScore THEN 1 ELSE 0
            END) AS Draws,
        SUM(CASE
              WHEN (Home = Team AND HomeScore < AwayScore)
                OR (Away = Team AND AwayScore < HomeScore)
              THEN 1 ELSE 0
            END) AS Losses,
        SUM(CASE WHEN Home = Team THEN HomeScore ELSE AwayScore END)    AS GoalsFor,
        SUM(CASE WHEN Home = Team THEN AwayScore ELSE HomeScore END)    AS GoalsAgainst,
        SUM(CASE
              WHEN (Home = Team AND HomeScore > AwayScore)
                OR (Away = Team AND AwayScore > HomeScore) THEN 3
              WHEN HomeScore = AwayScore THEN 1
              ELSE 0
            END) AS Points,
        SUM(CASE WHEN Home = Team THEN HomeScore ELSE AwayScore END)
        - SUM(CASE WHEN Home = Team THEN AwayScore ELSE HomeScore END)
        AS GoalDifference
      FROM (
        SELECT Home  AS Team, Home, Away, HomeScore, AwayScore, matchtype, Season
        FROM schedule
        WHERE HomeScore IS NOT NULL
          -- league filter: either ALL or exact match
          AND ( {league} = 'ALL'    OR matchtype = {league} )
          -- season filter: either ALL or exact match
          AND ( {season} = 'ALL'    OR Season    = {season} )
        
        UNION ALL
        
        SELECT Away  AS Team, Home, Away, HomeScore, AwayScore, matchtype, Season
        FROM schedule
        WHERE HomeScore IS NOT NULL
          AND ( {league} = 'ALL'    OR matchtype = {league} )
          AND ( {season} = 'ALL'    OR Season    = {season} )
      ) AS combined
      GROUP BY Team
      ORDER BY
        Points DESC,
        GoalDifference DESC,
        GoalsFor DESC
    ",
    league = league,
    season = season
  ) |>
    suppressWarnings()
}

#' @export
getSchedule <- function(league, season) {
  indexQuery(
    query = "
    SELECT
      IRLDate,
      MatchType,
      MatchDay,
      Home,
      Away,
      HomeScore,
      AwayScore,
      ExtraTime,
      Penalties,
      gid
    FROM schedule
    WHERE
      ( {season} = 'ALL'       OR season    = {season} )
      AND
      ( {league} = 'ALL'       OR MatchType = {league} )
    ORDER BY IRLDate;
  ",
    season = season,
    league = league
  )
}

#' @export
getPlayer <- function(pid) {
  portalQuery(
    "SELECT * 
    FROM allplayersview
    WHERE pid = {pid}",
    pid = pid
  ) |>
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 5))
    )
}

#' @export
getOrganizations <- function() {
  portalQuery(
    "SELECT o.ID, o.name AS organization, o.abbr AS abbreviation, t.name, t.primaryColor, t.secondaryColor, t.city
    FROM teams AS t
    LEFT JOIN organizations AS o ON t.orgID = o.ID
    ORDER BY o.ID"
  )
}

#' @export
getAcademyIndex <- function(outfield = TRUE, season) {
  if (outfield) {
    indexQuery(
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
      FROM academyoutfield WHERE season = {season};",
      season = season
    ) |>
      suppressWarnings()
  } else {
    indexQuery(
      "SELECT
          `name`, `club`, `apps`, `minutes played`, `average rating`, `player of the match`, won, lost, draw, `clean sheets`, conceded, `saves parried`, `saves held`,
          `saves tipped`, (1 - (conceded / (conceded + `saves parried` + `saves held` + `saves tipped`))) * 100 AS `save%`,
          `penalties faced`, `penalties saved`, `xsave%`, `xg prevented`
      FROM academykeeper WHERE season = {season};",
      season = season
    ) |>
      suppressWarnings()
  }
}

#' @export
getLeagueIndex <- function(
    outfield = TRUE, 
    season, 
    league = "ALL", 
    name = "ALL",
    career = FALSE
  ) {
  if (outfield) {
    if (career) {
      grouping <- "name, season"
    } else {
      grouping <- "name"
    }
    
    indexQuery(
      query = paste0("
        SELECT
          name, `club`, `position`, `apps`, `minutes played`,`average rating`, 
          `player of the match`, `distance run (km)`, `goals`,`assists`,`xg`,`shots on target`,
          `shots`, `penalties taken`,`penalties scored`,`successful passes`,`attempted passes`,
          `successful passes`/`attempted passes`*100 AS `pass%`,`key passes`,
          `successful crosses`,`attempted crosses`,`successful crosses`/`attempted crosses`*100 AS `cross%`,
          `chances created`,`successful headers`,`attempted headers`,
          `successful headers`/`attempted headers`*100 AS `header%`,`key headers`,
          `dribbles`,`tackles won`,`attempted tackles`,`tackles won`/`attempted tackles`*100 AS `tackle%`,
          `key tackles`,`interceptions`,`clearances`,`mistakes leading to goals`,
          `yellow cards`,`red cards`,`fouls`,`fouls against`,`offsides`,`xa`,
          `xg overperformance`,`fk shots`,`blocks`,`open play key passes`,
          `successful open play crosses`,`attempted open play crosses`,`shots blocked`,
          `progressive passes`,`successful presses`,`attempted presses`, `goals outside box`,
          CASE WHEN IFNULL(`attempted presses`,0)=0 THEN 0
               ELSE `successful presses`/`attempted presses`*100 END AS `press%`,
          CASE WHEN IFNULL(`attempted open play crosses`,0)=0 THEN 0
               ELSE `successful open play crosses`/`attempted open play crosses`*100 END AS `open play crosses%`,
          `shots on target`/`shots`*100 AS `shot accuracy%`,
          `xG` - 0.83*`penalties taken` AS `pen adj xG`, max_season
        FROM (
          SELECT
            name,
            GROUP_CONCAT(DISTINCT club SEPARATOR ', ')    AS `club`,
            MAX(`position`)                              AS `position`,
            SUM(`apps`)                                  AS `apps`,
            SUM(`minutes played`)                        AS `minutes played`,
            SUM(`distance run (km)`)                     AS `distance run (km)`,
            SUM(`goals`)                                 AS `goals`,
            SUM(`assists`)                               AS `assists`,
            SUM(`xg`)                                    AS `xg`,
            SUM(`shots on target`)                       AS `shots on target`,
            SUM(`shots`)                                 AS `shots`,
            SUM(`penalties taken`)                       AS `penalties taken`,
            SUM(`penalties scored`)                      AS `penalties scored`,
            SUM(`successful passes`)                     AS `successful passes`,
            SUM(`attempted passes`)                      AS `attempted passes`,
            SUM(`key passes`)                            AS `key passes`,
            SUM(`successful crosses`)                    AS `successful crosses`,
            SUM(`attempted crosses`)                     AS `attempted crosses`,
            SUM(`chances created`)                       AS `chances created`,
            SUM(`successful headers`)                    AS `successful headers`,
            SUM(`attempted headers`)                     AS `attempted headers`,
            SUM(`key headers`)                           AS `key headers`,
            SUM(`dribbles`)                              AS `dribbles`,
            SUM(`tackles won`)                           AS `tackles won`,
            SUM(`attempted tackles`)                     AS `attempted tackles`,
            SUM(`key tackles`)                           AS `key tackles`,
            SUM(`interceptions`)                         AS `interceptions`,
            SUM(`clearances`)                            AS `clearances`,
            SUM(`mistakes leading to goals`)             AS `mistakes leading to goals`,
            SUM(`yellow cards`)                          AS `yellow cards`,
            SUM(`red cards`)                             AS `red cards`,
            SUM(`fouls`)                                 AS `fouls`,
            SUM(`fouls against`)                         AS `fouls against`,
            SUM(`offsides`)                              AS `offsides`,
            SUM(`xa`)                                    AS `xa`,
            SUM(`xg overperformance`)                    AS `xg overperformance`,
            SUM(`fk shots`)                              AS `fk shots`,
            SUM(`blocks`)                                AS `blocks`,
            SUM(`open play key passes`)                  AS `open play key passes`,
            SUM(`successful open play crosses`)           AS `successful open play crosses`,
            SUM(`attempted open play crosses`)            AS `attempted open play crosses`,
            SUM(`shots blocked`)                         AS `shots blocked`,
            SUM(`progressive passes`)                     AS `progressive passes`,
            SUM(`successful presses`)                    AS `successful presses`,
            SUM(`attempted presses`)                     AS `attempted presses`,
            SUM(`goals outside box`)                     AS `goals outside box`,
            SUM(`player of the match`)                   AS `player of the match`,
            AVG(`average rating`)                        AS `average rating`,
            MAX(season)                                  AS `max_season`
          FROM (
            SELECT
              gd.`name`, gd.`club`, gd.`position`, gd.`apps`, gd.`minutes played`,
              gd.`distance run (km)`, gd.`average rating`, gd.`player of the match`,
              gd.`goals`, gd.`assists`, gd.`xg`, gd.`shots on target`, gd.`shots`,
              gd.`penalties taken`, gd.`penalties scored`, gd.`successful passes`,
              gd.`attempted passes`, gd.`key passes`, gd.`successful crosses`,
              gd.`attempted crosses`, gd.`chances created`, gd.`successful headers`,
              gd.`attempted headers`, gd.`key headers`, gd.`dribbles`, gd.`tackles won`,
              gd.`attempted tackles`, gd.`key tackles`, gd.`interceptions`,
              gd.`clearances`, gd.`mistakes leading to goals`, gd.`yellow cards`,
              gd.`red cards`, gd.`fouls`, gd.`fouls against`, gd.`offsides`, gd.`xa`,
              gd.`xg overperformance`, gd.`fk shots`, gd.`blocks`,
              gd.`open play key passes`, gd.`successful open play crosses`,
              gd.`attempted open play crosses`, gd.`shots blocked`,
              gd.`progressive passes`, gd.`successful presses`, gd.`attempted presses`,
              gd.`goals outside box`, s.season
            FROM `gamedataoutfield` AS gd
            JOIN schedule AS s ON gd.gid = s.gid
            WHERE
              ( {league} = 'ALL' OR s.MatchType = {league} )
              AND
              ( {season} = 'ALL' OR s.season    = {season} )
              AND
              ( {name} = 'ALL' OR gd.name       = {name} )
          ) AS q01
          GROUP BY ", grouping, "
        ) AS q02;"),
      league = league,
      season = season,
      name = name
    ) |>
      suppressWarnings()
  } else {
    if (career) {
      grouping <- "name, season"
    } else {
      grouping <- "name"
    }
    
    indexQuery(
      query = paste0("
        SELECT
          `name`, `club`, `apps`, `minutes played`, `average rating`,
          `player of the match`, won, lost, drawn,
          `clean sheets`, conceded,
          `saves parried`, `saves held`, `saves tipped`,
          (1 - (conceded / (
             conceded + `saves parried` + `saves held` + `saves tipped`))
          ) * 100                     AS `save%`,
          `penalties faced`, `penalties saved`,
          `xsave%`, `xg prevented`, max_season
        FROM (
          SELECT
            `name`,
            GROUP_CONCAT(DISTINCT club SEPARATOR ', ') AS `club`,
            SUM(`apps`)                             AS `apps`,
            SUM(`minutes played`)                   AS `minutes played`,
            AVG(`average rating`)                   AS `average rating`,
            SUM(`player of the match`)              AS `player of the match`,
            SUM(CASE
                  WHEN (Home = club AND HomeScore > AwayScore)
                     OR (Away = club AND AwayScore > HomeScore)
                  THEN 1 ELSE 0
                END)                                  AS won,
            SUM(CASE
                  WHEN (Home = club AND HomeScore < AwayScore)
                     OR (Away = club AND AwayScore < HomeScore)
                  THEN 1 ELSE 0
                END)                                  AS lost,
            SUM(CASE
                  WHEN HomeScore = AwayScore THEN 1 ELSE 0
                END)                                  AS drawn,
            SUM(`clean sheets`)                     AS `clean sheets`,
            SUM(`conceded`)                         AS conceded,
            SUM(`saves parried`)                    AS `saves parried`,
            SUM(`saves held`)                       AS `saves held`,
            SUM(`saves tipped`)                     AS `saves tipped`,
            SUM(`save%`)                            AS `save%`,
            SUM(`penalties faced`)                  AS `penalties faced`,
            SUM(`penalties saved`)                  AS `penalties saved`,
            AVG(`xsave%`)                           AS `xsave%`,
            SUM(`xg prevented`)                     AS `xg prevented`,
            MAX(season)                             AS `max_season`
          FROM (
            SELECT
              gd.`name`, gd.`club`, gd.`apps`, gd.`minutes played`,
              gd.`average rating`, gd.`player of the match`,
              gd.`clean sheets`, gd.`conceded`, gd.`saves parried`,
              gd.`saves held`, gd.`saves tipped`, gd.`save%`,
              gd.`penalties faced`, gd.`penalties saved`,
              gd.`xsave%`, gd.`xg prevented`, s.Home, s.Away,
              s.HomeScore, s.AwayScore, s.season
            FROM `gamedatakeeper` AS gd
            JOIN schedule AS s
              ON gd.gid = s.gid
            WHERE
              ( {league} = 'ALL' OR s.MatchType = {league} )
              AND
              ( {season} = 'ALL' OR s.season    = {season} )
              AND
              ( {name} = 'ALL' OR gd.name       = {name} )
          ) AS q01
          GROUP BY ", grouping, "
        ) AS q02;"),
      league = league,
      season = season,
      name = name
    ) |>
      suppressWarnings()
  }
}

#' @export
getSeasonalTotal <- function(outfield = TRUE, season) {
  if (outfield) {
    indexQuery(
      query = "
        SELECT
          name,
          club,
          SUM(apps)                    AS apps,
          SUM(`minutes played`)        AS `minutes played`,
          SUM(`distance run (km)`)     AS `distance run (km)`,
          AVG(`average rating`)        AS `average rating`,
          SUM(`player of the match`)   AS `player of the match`,
          SUM(goals)                   AS goals,
          SUM(assists)                 AS assists,
          SUM(xg)                      AS xg,
          SUM(`shots on target`)       AS `shots on target`,
          SUM(shots)                   AS shots,
          SUM(`penalties taken`)       AS `penalties taken`,
          SUM(`penalties scored`)      AS `penalties scored`,
          SUM(`successful passes`)     AS `successful passes`,
          SUM(`attempted passes`)      AS `attempted passes`,
          SUM(`pass%`)                 AS `pass%`,
          SUM(`key passes`)            AS `key passes`,
          SUM(`successful crosses`)    AS `successful crosses`,
          SUM(`attempted crosses`)     AS `attempted crosses`,
          SUM(`cross%`)                AS `cross%`,
          SUM(`chances created`)       AS `chances created`,
          SUM(`successful headers`)    AS `successful headers`,
          SUM(`attempted headers`)     AS `attempted headers`,
          SUM(`header%`)               AS `header%`,
          SUM(`key headers`)           AS `key headers`,
          SUM(dribbles)                AS dribbles,
          SUM(`tackles won`)           AS `tackles won`,
          SUM(`attempted tackles`)     AS `attempted tackles`,
          SUM(`tackle%`)               AS `tackle%`,
          SUM(`key tackles`)           AS `key tackles`,
          SUM(interceptions)           AS interceptions,
          SUM(clearances)              AS clearances,
          SUM(`mistakes leading to goals`) AS `mistakes leading to goals`,
          SUM(`yellow cards`)          AS `yellow cards`,
          SUM(`red cards`)             AS `red cards`,
          SUM(fouls)                   AS fouls,
          SUM(`fouls against`)         AS `fouls against`,
          SUM(offsides)                AS offsides,
          SUM(xa)                      AS xa,
          SUM(`xg overperformance`)    AS `xg overperformance`,
          SUM(`goals outside box`)     AS `goals outside box`,
          SUM(`fk shots`)              AS `fk shots`,
          SUM(blocks)                  AS blocks,
          SUM(`open play key passes`)  AS `open play key passes`,
          SUM(`successful open play crosses`)   AS `successful open play crosses`,
          SUM(`attempted open play crosses`)    AS `attempted open play crosses`,
          SUM(`shots blocked`)         AS `shots blocked`,
          SUM(`progressive passes`)     AS `progressive passes`,
          SUM(`successful presses`)     AS `successful presses`,
          SUM(`attempted presses`)      AS `attempted presses`
        FROM gamedataoutfield AS gd
        JOIN schedule AS s
          ON gd.gid = s.gid
        WHERE
          ( {season} = 'ALL' OR s.Season = {season} )
        GROUP BY
          name, club
        ORDER BY
          name, club;
      ",
      season = season
    )
  } else {
    indexQuery(
      query = "
        SELECT
          name,
          club,
          SUM(apps)                    AS apps,
          SUM(`minutes played`)        AS `minutes played`,
          AVG(`average rating`)        AS `average rating`,
          SUM(`player of the match`)   AS `player of the match`,
          SUM(`clean sheets`)          AS `clean sheets`,
          SUM(conceded)                AS conceded,
          SUM(`saves parried`)         AS `saves parried`,
          SUM(`saves held`)            AS `saves held`,
          SUM(`saves tipped`)          AS `saves tipped`,
          SUM(`save%`)                 AS `save%`,
          SUM(`penalties faced`)       AS `penalties faced`,
          SUM(`penalties saved`)       AS `penalties saved`,
          AVG(`xsave%`)                AS `xsave%`,
          SUM(`xg prevented`)          AS `xg prevented`
        FROM gamedatakeeper AS gd
        JOIN schedule AS s
          ON gd.gid = s.gid
        WHERE
          ( {season} = 'ALL' OR s.Season = {season} )
        GROUP BY
          name, club
        ORDER BY
          name, club;
      ",
      season = season
    )
  }
}

#' @export
getNextGameID <- function(season) {
  indexQuery(
    query = "
      SELECT team, MIN(gid) AS gid
      FROM (
        SELECT home AS team, MIN(s.gid) AS gid
        FROM schedule s
        LEFT JOIN gamedataoutfield o
          ON s.gid = o.gid
        WHERE o.gid IS NULL
          AND s.season    = {season}
          AND s.Matchtype >= 0
        GROUP BY home

        UNION ALL

        SELECT away AS team, MIN(s.gid) AS gid
        FROM schedule s
        LEFT JOIN gamedataoutfield o
          ON s.gid = o.gid
        WHERE o.gid IS NULL
          AND s.season    = {season}
          AND s.Matchtype >= 0
        GROUP BY away
      ) AS combined
      GROUP BY team
      ORDER BY gid;
    ",
    season = season
  )
}

#' @export
getManagers <- function() {
  portalQuery(
    "SELECT organizations.id, teams.name, teams.primaryColor, managers.orgManager, managers.assManager1, managers.assManager2 
      FROM organizations
      LEFT JOIN managers ON organizations.id = managers.orgID
      LEFT JOIN teams ON organizations.id = teams.orgID
      WHERE teams.affiliate = 1 AND organizations.id >= 0;"
  )
}

#' @export
getUnapprovedPlayers <- function() {
  portalQuery(
    "SELECT pid, username, first, last, tpe, tpebank, render
    FROM allplayersview
    WHERE status_p = -1;"
  )
}