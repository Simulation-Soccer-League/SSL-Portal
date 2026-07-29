# nolint: line_length_linter
box::use(
  cachem[cache_mem],
  dplyr[across, if_else, mutate, where],
  lubridate,
  memoise[memoise],
  tidyr[replace_na],
)

box::use(
  app / logic / db / database[indexQuery, portalQuery],
)

# Alternative memoised function for heavy calls
memoisedIndexQuery <- 
  memoise(
    indexQuery, 
    cache = cache_mem(max_age = 60*30)
  )

# Alternative memoised function for heavy calls}
memoisedPortalQuery <- 
  memoise(
    portalQuery,
    cache = cache_mem(max_age = 60*10)
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
    "SELECT Time, Player, Username, Source, Transaction
    FROM bankhistoryview
    WHERE pid = {pid} AND Status = 1
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
  memoisedPortalQuery(
    "SELECT name, username, position, pid
    FROM allplayersview
    ORDER BY created DESC
    LIMIT 10;"
  )
}

#' @export
getTopEarners <- function() {
  memoisedPortalQuery(
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
getActivePid <- function(uid) {
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

  memoisedPortalQuery(
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
getOrganizationPlayers <- function(oid) {
  memoisedPortalQuery(
    "SELECT p.*
      FROM allplayersview p
      LEFT JOIN organizations o ON
        p.organization = o.name
      WHERE o.id = {oid};",
    oid = oid
  ) |> 
    suppressWarnings()
}

#' @export
getTeamInformation <- function(oid){
  memoisedPortalQuery(
    "SELECT t.*, muid.orgManager, muid.assManager1, 
      muid.assManager2, m.om, m.am1, m.am2
      FROM teams t
      LEFT JOIN managers muid ON t.orgID = muid.orgID
      LEFT JOIN managerview m ON t.orgID = m.orgID
      WHERE t.orgID = {oid};",
    oid = oid
  )
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
getManagedPlayers <- function(uid) {
  portalQuery(
    "SELECT *
    FROM allplayersview
    WHERE organization = (
      SELECT name
      FROM organizations
      WHERE id = (
        SELECT orgID
        FROM managers
        WHERE orgManager = {uid} OR assManager1 = {uid} OR assManager2  = {uid}
      )
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
  
  
  memoisedPortalQuery(
    query = 
      "SELECT t.name AS teamName, t.affiliate AS currentAffiliate, wb.*, uh.attribute as Attribute, uh.old, uh.new, 
      nat.fmID AS nationalityID
        FROM playerdata pd
        LEFT JOIN weeklybuilds wb ON pd.pid = wb.pid
        JOIN updatehistory uh ON pd.pid = uh.pid
        LEFT JOIN nationality nat ON 
          (pd.nationality = nat.abbreviation) OR 
          (pd.nationality = nat.name)
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
      memoisedIndexQuery(
        "SELECT season 
        FROM seasoninfo 
        ORDER BY season DESC 
        LIMIT 1;") |> 
      unlist() + 1
  }

  memoisedPortalQuery(
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
  memoisedPortalQuery(
    "SELECT o.ID, o.name AS organization, t.abbreviation AS abbreviation, t.name, t.primaryColor, t.secondaryColor, t.city
    FROM teams AS t
    LEFT JOIN organizations AS o ON t.orgID = o.ID
    ORDER BY o.ID, t.affiliate;"
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
    "SELECT pid, username, first, last, tpe, tpebank, render, position
    FROM allplayersview
    WHERE status_p = -1;"
  )
}

#' @export
getAChistory <- function(){
  memoisedPortalQuery(
    "SELECT 
      CONCAT(
        'W',
          FLOOR(
          DATEDIFF(
            CONVERT_TZ(FROM_UNIXTIME(time), 'UTC', 'America/Los_Angeles'),
            '2024-07-22' 
          ) / 7
        ) + 140
      ) AS nweeks,
      COUNT(*) AS count
    FROM tpehistory
    WHERE source = 'Activity Check'
    GROUP BY nweeks
    ORDER BY nweeks;"
  )
}


