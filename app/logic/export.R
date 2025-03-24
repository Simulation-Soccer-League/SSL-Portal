box::use(
  bit64[as.integer64],
  dplyr[if_else, select, mutate, case_when],
  stringr[
    str_remove_all,
    str_replace, 
    str_replace_all,
    str_split, 
    str_to_lower, 
    str_to_title,
    str_to_upper,
  ]
)

box::use(
  app/logic/constant
)

#' @export
downloadPlayer <- function(temp){
  traits <- 
    temp$traits |> 
    str_split(pattern = constant$traitSep, simplify = TRUE) |> 
    str_to_lower()
  
  traits <- 
    ((constant$jsonTraits |> str_to_lower()) %in% traits) |> 
    which()
  
  traits <- 
    sum(as.integer64(2)^(traits-1))
  
  gkAtt <- 
    sapply(
      temp |> 
        select(`aerial reach`:throwing),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  mentalAtt <- 
    sapply(
      temp |> 
        select(`aggression`:`work rate`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    ) 
  
  physicalAtt <- 
    sapply(
      temp |> 
        select(`acceleration`:`strength`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  technicalAtt <- 
    sapply(
      temp |> 
        select(`corners`:`technique`),
      FUN = function(x) {if_else(is.na(x), 5, x)}
    )
  
  positions <- 
    sapply(
      temp |> 
        select(`pos_st`:`pos_gk`),
      FUN = function(x) {if_else(is.na(x)|x == 0, 1, as.numeric(x))}
    ) 
  
  paste(
    '{"GoalKeeperAttributes":{
          ',
    paste(paste('"', names(gkAtt) |> str_to_title(), '"', sep = ""), gkAtt, sep = ":", collapse = ",") |> 
    str_replace_all(pattern = " ", replacement = "") |> 
    str_replace(pattern = "TendencyToRush", replacement = "RushingOut") |> 
    str_replace(pattern = "AerialReach", replacement = "AerialAbility"),
    '},
"MentalAttributes":{
          ',
    paste(paste('"', names(mentalAtt) |> str_to_title(), '"', sep = ""), mentalAtt, sep = ":", collapse = ",") |> 
    str_replace_all(pattern = " ", replacement = "") |> 
    str_replace(pattern = "WorkRate", replacement = "Workrate"),
    '},
"PhysicalAttributes":{
          ',
    paste(paste('"', names(physicalAtt) |> str_to_title(), '"', sep = ""), physicalAtt, sep = ":", collapse = ",") |> 
    str_replace_all(pattern = " ", replacement = "") |> 
    str_replace(pattern = "JumpingReach", replacement = "Jumping"),
    ',"LeftFoot":', temp$`left foot`,
    ',"RightFoot":', temp$`right foot`,
    '},
"TechnicalAttributes":{
          ',
    paste(paste('"', names(technicalAtt) |> str_to_title(), '"', sep = ""), technicalAtt, sep = ":", collapse = ",") |> 
    str_replace_all(pattern = " ", replacement = "") |> 
    str_replace(pattern = "FreeKick", replacement = "Freekicks") |> 
    str_replace(pattern = "LongThrows", replacement = "Longthrows"),
    '},
"Positions":{
          ',
      paste(
        paste('"', 
              constant$positionsGK[
                sapply(
                  (names(positions) |> 
                     str_remove_all(pattern = "pos_") |> 
                     str_to_upper()
                   ), 
                  FUN = function(x) which(names(constant$positionsGK) == x)) |> unlist()
                ], '"', sep = ""), 
        positions, 
        sep = ":", 
        collapse = ","
      ) |> 
      str_replace_all(pattern = " ", replacement = "") |> 
      str_replace_all(pattern = "AttackingMidfielder", replacement = "AttackingMid") |> 
      str_replace_all(pattern = "Wingback", replacement = "WingBack") |> 
      str_replace_all(pattern = "(Left|Right|Central)([A-Za-z]+)(\":\"?[0-9]+)", replacement = "\\2\\1\\3"),
    '},
"HairColour":"', 
    if_else(temp$hair_color |> nchar() == 4,
            constant$hairColor[temp$hair_color] |> 
              names() |> 
              str_replace_all(pattern = " |\\(|\\)", replacement = ""),
            temp$hair_color |> 
              str_replace_all(pattern = " |\\(|\\)", replacement = "")
            ), 
    '","HairLength":"', temp$hair_length |> str_to_title(), 
    '","SkinColour":', temp$skintone, 
    ',"Height":', 
    if_else(
      !is.na(temp$height |> as.numeric()), 
      (temp$height |> as.numeric()*2.54) |> as.character(), 
      temp$height |> as.character()
    ),
    ',"Weight":', 
    if_else(
      !is.na(temp$weight |> as.numeric()), 
      (temp$weight |> as.numeric()*0.453592) |> as.character(), 
      temp$weight |> as.character()
    ),
    ',"PreferredMoves":', traits,
    ', "Nation.Id":', temp$nationalityID,
    ',"Born":"', 
    ## OLD
    # ("2004-07-01" |> as_date()) - years(currentSeason$season - (temp$class |> str_extract(pattern = "[0-9]+") |> as.numeric())) ,
    ## NEW: Changed to give all players the same age
    "2000-01-01",
    '","DocumentType":"Player"}',
    sep = ""
  )
}