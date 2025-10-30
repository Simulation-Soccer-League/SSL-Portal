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
  
  temp <- 
    temp |> 
    mutate(
      nationalityID = case_when(
        nationality == "Afghanistan" ~ 106,
        nationality == "Albania" ~ 752,
        nationality == "Algeria" ~ 5,
        nationality == "Andorra" ~ 753,
        nationality == "Angola" ~ 6,
        nationality == "Antigua and Barbuda" ~ 359,
        nationality == "Argentina" ~ 1649,
        nationality == "Armenia" ~ 754,
        nationality == "Australia" ~ 1435,
        nationality == "Austria" ~ 755,
        nationality == "Azerbaijan" ~ 756,
        nationality == "Bahamas" ~ 388,
        nationality == "Bahrain" ~ 107,
        nationality == "Bangladesh" ~ 108,
        nationality == "Barbados" ~ 361,
        nationality == "Belarus" ~ 758,
        nationality == "Belgium" ~ 757,
        nationality == "Belize" ~ 362,
        nationality == "Benin" ~ 7,
        nationality == "Bhutan" ~ 131012,
        nationality == "Bolivia" ~ 1650,
        nationality == "Bosnia and Herzegovina" ~ 759,
        nationality == "Botswana" ~ 8,
        nationality == "Brazil" ~ 1651,
        nationality == "Brunei" ~ 109,
        nationality == "Bulgaria" ~ 760,
        nationality == "Burkina Faso" ~ 9,
        nationality == "Burundi" ~ 10,
        nationality == "Cabo Verde" ~ 12,
        nationality == "Cambodia" ~ 118,
        nationality == "Cameroon" ~ 11,
        nationality == "Canada" ~ 364,
        nationality == "Central African Republic" ~ 13,
        nationality == "Chad" ~ 14,
        nationality == "Chile" ~ 1652,
        nationality == "China" ~ 110,
        nationality == "Colombia" ~ 1653,
        nationality == "Comoros" ~ 919586,
        nationality == "Côte d'Ivoire" ~ 24,
        nationality == "Democratic Republic of the Congo" ~ 53,
        nationality == "Republic of the Congo" ~ 49,
        nationality == "Costa Rica" ~ 366,
        nationality == "Croatia" ~ 761,
        nationality == "Cuba" ~ 367,
        nationality == "Cyprus" ~ 762,
        nationality == "Czechia" ~ 763,
        nationality == "Denmark" ~ 764,
        nationality == "Djibouti" ~ 15,
        nationality == "Dominica" ~ 368,
        nationality == "Dominican Republic" ~ 142527,
        nationality == "East Timor" ~ 5626837,
        nationality == "England" ~ 765,
        nationality == "Ecuador" ~ 1654,
        nationality == "Egypt" ~ 16,
        nationality == "El Salvador" ~ 370,
        nationality == "Equatorial Guinea" ~ 17,
        nationality == "Eritrea" ~ 129511,
        nationality == "Estonia" ~ 766,
        nationality == "Eswatini" ~ 47,
        nationality == "Ethiopia" ~ 18,
        nationality == "Faroe Islands" ~ 767,
        nationality == "Fiji" ~ 1437,
        nationality == "Finland" ~ 768,
        nationality == "France" ~ 769,
        nationality == "Gabon" ~ 19,
        nationality == "Gambia" ~ 20,
        nationality == "Georgia" ~ 770,
        nationality == "Germany" ~ 771,
        nationality == "Ghana" ~ 21,
        nationality == "Gibraltar" ~ 214394,
        nationality == "Greece" ~ 772,
        nationality == "Grenada" ~ 371,
        nationality == "Guatemala" ~ 373,
        nationality == "Guinea" ~ 22,
        nationality == "Guinea-Bissau" ~ 23,
        nationality == "Guyana" ~ 374,
        nationality == "Haiti" ~ 375,
        nationality == "Honduras" ~ 376,
        nationality == "Hungary" ~ 773,
        nationality == "Iceland" ~ 774,
        nationality == "India" ~ 112,
        nationality == "Indonesia" ~ 113,
        nationality == "Iran" ~ 114,
        nationality == "Iraq" ~ 115,
        nationality == "Ireland" ~ 789,
        nationality == "Israel" ~ 775,
        nationality == "Italy" ~ 776,
        nationality == "Jamaica" ~ 377,
        nationality == "Japan" ~ 116,
        nationality == "Jordan" ~ 117,
        nationality == "Kazakhstan" ~ 119,
        nationality == "Kenya" ~ 25,
        nationality == "Kiribati" ~ 209002,
        nationality == "Korea, North" ~ 129,
        nationality == "Korea, South" ~ 135,
        nationality == "Kosovo" ~ 217945,
        nationality == "Kuwait" ~ 120,
        nationality == "Kyrgyzstan" ~ 121,
        nationality == "Laos" ~ 122,
        nationality == "Latvia" ~ 777,
        nationality == "Lebanon" ~ 123,
        nationality == "Lesotho" ~ 26,
        nationality == "Liberia" ~ 27,
        nationality == "Libya" ~ 28,
        nationality == "Liechtenstein" ~ 778,
        nationality == "Lithuania" ~ 779,
        nationality == "Luxembourg" ~ 780,
        nationality == "Madagascar" ~ 29,
        nationality == "Malawi" ~ 30,
        nationality == "Malaysia" ~ 125,
        nationality == "Maldives" ~ 126,
        nationality == "Mali" ~ 31,
        nationality == "Malta" ~ 782,
        nationality == "Marshall Islands" ~ 15064643,
        nationality == "Mauritania" ~ 32,
        nationality == "Mauritius" ~ 33,
        nationality == "Mexico" ~ 379,
        nationality == "Micronesia" ~ 15064643,
        nationality == "Moldova" ~ 783,
        nationality == "Monaco" ~ 769,
        nationality == "Mongolia" ~ 129505,
        nationality == "Montenegro" ~ 62002127,
        nationality == "Morocco" ~ 34,
        nationality == "Mozambique" ~ 35,
        nationality == "Myanmar" ~ 127,
        nationality == "Namibia" ~ 36,
        nationality == "Nauru" ~ 15064643,
        nationality == "Nepal" ~ 128,
        nationality == "Netherlands" ~ 784,
        nationality == "New Zealand" ~ 1438,
        nationality == "Nicaragua" ~ 381,
        nationality == "Niger" ~ 37,
        nationality == "Nigeria" ~ 38,
        nationality == "North Macedonia" ~ 781,
        nationality == "Northern Ireland" ~ 785,
        nationality == "Norway" ~ 786,
        nationality == "Oman" ~ 130,
        nationality == "Pakistan" ~ 131,
        nationality == "Palau" ~ 15064643,
        nationality == "Panama" ~ 382,
        nationality == "Papua New Guinea" ~ 1439,
        nationality == "Paraguay" ~ 1655,
        nationality == "Peru" ~ 1656,
        nationality == "Philippines" ~ 141,
        nationality == "Poland" ~ 787,
        nationality == "Portugal" ~ 788,
        nationality == "Qatar" ~ 132,
        nationality == "Romania" ~ 790,
        nationality == "Russia" ~ 791,
        nationality == "Rwanda" ~ 39,
        nationality == "Saint Kitts and Nevis" ~ 385,
        nationality == "Saint Lucia" ~ 384,
        nationality == "Saint Vincent and the Grenadines" ~ 386,
        nationality == "Samoa" ~ 1444,
        nationality == "San Marino" ~ 792,
        nationality == "Sao Tome and Principe" ~ 40,
        nationality == "Saudi Arabia" ~ 133,
        nationality == "Scotland" ~ 793,
        nationality == "Senegal" ~ 41,
        nationality == "Serbia" ~ 802,
        nationality == "Seychelles" ~ 42,
        nationality == "Sierra Leone" ~ 43,
        nationality == "Singapore" ~ 134,
        nationality == "Slovakia" ~ 794,
        nationality == "Slovenia" ~ 795,
        nationality == "Solomon Islands" ~ 1440,
        nationality == "Somalia" ~ 44,
        nationality == "South Africa" ~ 45,
        nationality == "South Sudan" ~ 13113220,
        nationality == "Spain" ~ 796,
        nationality == "Sri Lanka" ~ 136,
        nationality == "Sudan" ~ 46,
        nationality == "Suriname" ~ 387,
        nationality == "Sweden" ~ 797,
        nationality == "Switzerland" ~ 798,
        nationality == "Syria" ~ 137,
        nationality == "Taiwan" ~ 138,
        nationality == "Tajikistan" ~ 139,
        nationality == "Tanzania" ~ 48,
        nationality == "Thailand" ~ 140,
        nationality == "Togo" ~ 50,
        nationality == "Tonga" ~ 1442,
        nationality == "Trinidad and Tobago" ~ 389,
        nationality == "Tunisia" ~ 51,
        nationality == "Turkey" ~ 799,
        nationality == "Turkmenistan" ~ 142,
        nationality == "Tuvalu" ~ 23088616,
        nationality == "Uganda" ~ 52,
        nationality == "Ukraine" ~ 800,
        nationality == "United Arab Emirates" ~ 143,
        nationality == "United States" ~ 390,
        nationality == "Uruguay" ~ 1657,
        nationality == "Uzbekistan" ~ 144,
        nationality == "Vanuatu" ~ 1443,
        nationality == "Vatican City" ~ 776,
        nationality == "Venezuela" ~ 1658,
        nationality == "Vietnam" ~ 145,
        nationality == "Wales" ~ 801,
        nationality == "Yemen" ~ 146,
        nationality == "Zambia" ~ 54,
        nationality == "Zimbabwe" ~ 55,
        TRUE ~ NA_integer_
      )
    )
  
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
            constant$hairColor[constant$hairColor == temp$hair_color] |> 
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
