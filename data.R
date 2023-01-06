library(RSQLite)
library(tidyverse)
library(httr)
library(jsonlite)
library(baseballr)

db <- dbConnect(SQLite(), "app.db")

start.year <- 2020
end.year <- as.integer(format(Sys.Date(), "%Y"))
years <- seq(start.year, end.year)

# Helper Functions --------------------------------------------------------
has <- function(string, patterns) {
  for (pattern in patterns) {
    if (grepl(pattern, string, ignore.case=TRUE))
      return(TRUE);
  }
  FALSE;
}

categorize_injury <- function(injuries_list) {
  new_injuries_list <- c();
  for (injury in injuries_list) {
    body_part <- "";
    if (has(injury, c("illness", "covid", "flu", "viral", "mononucleosis",
                      "esophageal", "gastr", "append", "vertigo", "colon",
                      "testicular", "kidney", "heart", "tumor", "colitis"))) {
      body_part <- "Illness/Internal";
    } else if (has(injury, c("tommy"))) {
      body_part <- "Tommy John Surgery";
    } else if (has(injury, c("elbow"))) {
      body_part <- "Elbow (non-Tommy John)";
    } else if (has(injury, c("shoulder", "rotator cuff", "teres major",
                             "ac joint", "sc joint"))) {
      body_part <- "Shoulder/Rotator Cuff";
    } else if (has(injury, c("forearm", "flexor", "ulna", "radial"))) {
      body_part <- "Flexor/Forearm";
    } else if (has(injury, c("bicep", "tricep"))) {
      body_part <- "Upper Arm Muscle";
    } else if (has(injury, c("wrist", "hand", "carpal", "hamate"))) {
      body_part <- "Wrist/Hand";
    } else if (has(injury, c("hip", "si joint", "groin"))) {
      body_part <- "Hip/Groin";
    } else if (has(injury, c("femur"))) {
      body_part <- "Upper Leg Bone";
    } else if (has(injury, c("quad", "hamstring", "thigh", "glute",
                             "adductor"))) {
      body_part <- "Upper Leg Muscle";
    } else if (has(injury, c("calf", "lower leg"))) {
      body_part <- "Lower Leg Muscle";
    } else if (has(injury, c("arm"))) {
      body_part <- "Arm (other)";
    } else if (has(injury, c("leg", "shin", "fibula", "tibia"))) {
      body_part <- "Leg (other)";
    } else if (has(injury, c("ankle", "achilles", "foot", "heel", "plantar"))) {
      body_part <- "Ankle/Foot/Heel";
    } else if (has(injury, c("knee", "torn acl", "patellar"))) {
      body_part <- "Knee";
    } else if (has(injury, c("chest", "pectoral"))) {
      body_part <- "Chest";
    } else if (has(injury, c("back", "lumbar", "vertebra", "spine"))) {
      body_part <- "Back";
    } else if (has(injury, c("lat"))) {
      body_part <- "Lat";
    } else if (has(injury, c("oblique", "abdom", "core", "hernia", "side"))) {
      body_part <- "Oblique/Abdominal";
    } else if (has(injury, c("neck", "thoracic outlet", "cervical"))) {
      body_part <- "Neck/Thoracic Outlet";
    } else if (has(injury, c("rib", "intercostal"))) {
      body_part <- "Rib";
    } else if (has(injury, c("finger", "thumb", "blister"))) {
      body_part <- "Finger";
    } else if (has(injury, c("toe"))) {
      body_part <- "Toe";
    } else if (has(injury, c("concussion", "head", "facial", "mouth", "nose",
                             "nasal", "eye"))) {
      body_part <- "Head/Face";
    } else {
      body_part <- "Undisclosed";
    }
    new_injuries_list <- c(new_injuries_list, body_part);
  }
  new_injuries_list;
}

# Data Fetch Functions ----------------------------------------------------
fetch_injury_data <- function(years) {
  expected.columns <- c("date", "dateDisplay", "eligibledate", "injurySurgery",
                        "latestUpdate", "loaddate", "mlbamid", "playerId",
                        "playerName", "playerNameRoute", "position",
                        "retrodate", "returndate", "season", "status", "team")
  injury.df <- data.frame(matrix(nrow = 0, ncol = length(expected.columns)))
  colnames(injury.df) <- expected.columns
  
  for (year in years) {
    cat(paste("Fetching injury data for", year), sep = "\n")
    fangraphs.base_url <- "https://www.fangraphs.com/api/roster-resource/injury-report/"
    fangraphs.header <- add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36",
      "Referer" ="https://www.fangraphs.com/roster-resource/injury-report"
    )
    
    loaddate.request <- GET(
      paste(fangraphs.base_url, "loaddate", sep = ""),
      query = list(season = year),
      fangraphs.header
    )
    loaddate <- content(loaddate.request, as = "text")
    
    injuries.request <- GET(
      paste(fangraphs.base_url, "data", sep = ""),
      query = list(loaddate = loaddate, season = year),
      fangraphs.header
    )
    injuries <- content(injuries.request, as = "text")
    injury.df <- rbind(
      injury.df,
      subset(
        fromJSON(injuries),
        select = expected.columns
      )
    )
  }
  
  # Format columns
  injury.df %>%
    drop_na(playerId) %>%
    mutate(
      injuryCategory = categorize_injury(injurySurgery),
      date = as.Date(date, format = "%m/%d/%y"),
      retrodate = as.Date(retrodate, format = "%m/%d/%y"),
      returndate = as.Date(returndate, format = "%m/%d/%y"),
      eligibledate = format(
        as.Date(eligibledate, format = "%m/%d/%y"),
        "%Y-%m-%d"
      ),
      date = pmin(
        if_else(
          as.integer(format(date, "%Y")) >= 2019,
          date,
          # Correction for typo Jean Segura injury on "06/15/02"
          as.Date(
            paste(
              format(date, "%m/%d"),
              format(retrodate, "%y"),
              sep = "/"
            ),
            format = "%m/%d/%y"
          )
        ),
        retrodate,
        na.rm = TRUE
      ),
      injuryDuration = returndate - date,
      date = format(date, "%Y-%m-%d"),
      retrodate = format(retrodate, "%Y-%m-%d"),
      returndate = format(returndate, "%Y-%m-%d")
    ) %>%
    separate(position, c("position1", "position2"), "/", remove = FALSE,
             extra = "drop", fill = "right")
}

fetch_mlb_teams <- function(years) {
  team.colors <- c(
    ARI = "#a71930", ATL = "#ce1141", BAL = "#df4601", BOS = "#bd3039",
    CHC = "#cc3433", CHW = "#000000", CIN = "#c6011f", CLE = "#e31937",
    COL = "#333366", DET = "#0c2c56", HOU = "#002d62", KCR = "#004687",
    LAA = "#ba0021", LAD = "#005a9c", MIA = "#ff6600", MIL = "#0a2351",
    MIN = "#002b5c", NYM = "#ff5910", NYY = "#003087", OAK = "#003831",
    PHI = "#284898", PIT = "#fdb827", SDP = "#fdb827", SFG = "#fd5a1e",
    SEA = "#0c2c56", STL = "#c41e3a", TBR = "#092c5c", TEX = "#c0111f",
    TOR = "#134a8e", WSN = "#ab0003"
  )
  
  teams.df <- data.frame()
  for (year in years) {
    cat(paste("Fetching teams data for", year), sep = "\n")
    statsapi.teams.request <- GET(
      "https://statsapi.mlb.com/api/v1/teams",
      query = list(lang = "en", sportId = 1, season = year)
    )
    teams.json <- content(statsapi.teams.request, as = "text")
    teams.df <- rbind(
      teams.df,
      fromJSON(teams.json)$teams %>%
        select(season, id, name, abbreviation)
    )
  }
  teams.df %>%
    mutate(
      fangraphsAbbreviation = ifelse(
        abbreviation == "WSH", "WSN",
        ifelse(abbreviation == "SD", "SDP",
          ifelse(abbreviation == "SF", "SFG",
            ifelse(abbreviation == "TB", "TBR",
              ifelse(abbreviation == "CWS", "CHW",
                ifelse(abbreviation == "AZ", "ARI",
                  ifelse(abbreviation == "KC", "KCR",
                         abbreviation
                  )
                )
              )
            )
          )
        )
      ),
      primaryColor = unlist(team.colors)[fangraphsAbbreviation]
    )
}

# Write Tables ------------------------------------------------------------
dbWriteTable(
  db,
  "injuries",
  fetch_injury_data(years),
  overwrite = TRUE
)
dbWriteTable(
  db,
  "teams",
  fetch_mlb_teams(years),
  overwrite = TRUE
)
# dbWriteTable(
#   db,
#   "batter_seasons",
#   fg_batter_leaders(x = start.year, y = end.year, qual = 0)
# )
# dbWriteTable(
#   db,
#   "pitcher_seasons",
#   fg_pitch_leaders(x = start.year, y = end.year, qual = 0)
# )