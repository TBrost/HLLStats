
# Apply JSON conversion and nest weapon data
data_csv_nested <- data_csv %>%
  mutate(
    Weapons = map(Weapons, json_to_nested_df),
    DeathsbyWeapons = map(`Death by Weapons`, json_to_nested_df)
  )

# --- 2. Team Assignment ---

# Determine team based on most frequent death weapon and kill weapon
team_d <- data_csv_nested %>%
  select(`Player ID`, DeathsbyWeapons) %>%
  unnest_longer(DeathsbyWeapons) %>% # Efficiently unnest lists
  group_by(`Player ID`) %>%
  slice_max(order_by = Count, n = 1, with_ties = FALSE) %>% # More robustly get max
  ungroup() %>%
  mutate(team_d = factor(case_when(
    Weapon %in% US_WEAPONS ~ "AXIS", # Reversed logic as deaths are from enemy weapons
    Weapon %in% AXIS_WEAPONS ~ "ALLIES",
    TRUE ~ NA_character_ # Explicit NA character
  ))) %>%
  select(`Player ID`, team_d)

team_k <- data_csv_nested %>%
  select(`Player ID`, Weapons) %>%
  unnest_longer(Weapons) %>% # Efficiently unnest lists
  group_by(`Player ID`) %>%
  slice_max(order_by = Count, n = 1, with_ties = FALSE) %>% # More robustly get max
  ungroup() %>%
  mutate(team_k = factor(case_when(
    Weapon %in% US_WEAPONS ~ "ALLIES",
    Weapon %in% AXIS_WEAPONS ~ "AXIS",
    TRUE ~ NA_character_ # Explicit NA character
  ))) %>%
  select(`Player ID`, team_k)

team <- full_join(team_d, team_k, by = "Player ID") %>%
  mutate(team = factor(case_when(
    is.na(team_d) & is.na(team_k) ~ NA_character_,
    !is.na(team_d) & is.na(team_k) ~ team_d,
    is.na(team_d) & !is.na(team_k) ~ team_k,
    team_d == team_k ~ team_d,
    TRUE ~ NA_character_
  ))) %>%
  select(`Player ID`, team)

data_csv2 <- left_join(data_csv, team, by = "Player ID")

data_csv_nested2 <- data_csv2 %>%
  mutate(Weapons = map(Weapons, json_to_nested_df)) # Re-apply JSON conversion - seems redundant, check if needed

# --- 3. Kill and Death Type Analysis ---

# Death Types Calculation
DeathTypes <- data_csv_nested %>% # Use data_csv_nested, not re-nested data_csv_nested2 if no changes were intended
  select(`Player ID`, DeathsbyWeapons) %>%
  unnest_longer(DeathsbyWeapons) %>%
  mutate(DeathType = case_when(
    Weapon %in% Arty ~ "Deaths (Arty)",
    Weapon %in% Armor ~ "Deaths (Armor)",
    TRUE ~ "Deaths (Inf)"
  )) %>%
  group_by(`Player ID`, DeathType) %>%
  summarise(total = sum(Count), .groups = 'drop') %>% # Explicitly drop groups for efficiency
  pivot_wider(names_from = DeathType, values_from = total, values_fill = 0) # Explicit fill

# Kill Types Calculation
KillTypes <- data_csv_nested2 %>% # Using data_csv_nested2 which is data_csv2 nested again.
  select(Name, Weapons, team) %>%
  unnest_longer(Weapons) %>%
  mutate(WeaponType = factor(case_when(
    Weapon %in% semiAuto ~ "SemiAuto",
    Weapon %in% BoltAction ~ "Bolt-Action",
    Weapon %in% MG ~ "MG",
    Weapon %in% Automatic ~ "Automatic",
    Weapon %in% Arty ~ "Arty",
    Weapon %in% Armor ~ "Armor",
    Weapon %in% Grenade ~ "Grenade",
    Weapon %in% AT ~ "AT",
    Weapon %in% Satchel ~ "Satchel",
    Weapon %in% AT_Gun ~ "AT-Gun",
    Weapon %in% Sniper ~ "Sniper",
    Weapon %in% Mine ~ "Mine",
    Weapon %in% Melee ~ "Melee",
    Weapon %in% Command ~ "Command",
    TRUE ~ "Other"
  )))

# Armor Kills - Top 10
ArmorKills <- KillTypes %>%
  filter(Weapon %in% Armor) %>%
  group_by(team, Name) %>%
  summarise(total = sum(Count), .groups = 'drop') %>%
  slice_max(order_by = total, n = 10, with_ties = FALSE)

# Kill Types Summary by Team and Weapon Type
KillTypesSummary <- KillTypes %>%
  group_by(team, WeaponType) %>%
  summarise(total = sum(Count), .groups = 'drop')

# --- 4. Team and KD Calculations ---

# Team Kill/Death Summary
killteam_summary <- data_csv_nested2 %>%
  group_by(team) %>%
  summarise(Kills = sum(Kills), Deaths = sum(Deaths), .groups = 'drop')

# Kill Types Summary by Team and Weapon Category (Infantry/Armor/Arty/Command)
KillCategorySummary <- KillTypes %>%
  mutate(WeaponCategory = case_when(
    WeaponType == "Armor" ~ "Armor",
    WeaponType == "Arty" ~ "Arty",
    WeaponType == "Command" ~ "Command",
    TRUE ~ "Infantry"
  )) %>%
  group_by(team, WeaponCategory) %>%
  summarise(Kills = sum(Count, na.rm = TRUE), .groups = 'drop')

# KD Calculations
KD_Kills <- KillCategorySummary %>%
  pivot_wider(names_from = WeaponCategory, values_from = Kills, values_fill = 0)

calculate_kd <- function(x, y) {
  round(x / y, 2)
}

AXIS_KD <- calculate_kd(rowSums(KD_Kills[KD_Kills$team == "AXIS", -1]), rowSums(KD_Kills[KD_Kills$team == "ALLIES", -1]))
ALLIES_KD <- calculate_kd(rowSums(KD_Kills[KD_Kills$team == "ALLIES", -1]), rowSums(KD_Kills[KD_Kills$team == "AXIS", -1]))
AXIS_KD_ADJ <- calculate_kd(KD_Kills$Infantry[KD_Kills$team == "AXIS"], KD_Kills$Infantry[KD_Kills$team == "ALLIES"])
ALLIES_KD_ADJ <- calculate_kd(KD_Kills$Infantry[KD_Kills$team == "ALLIES"], KD_Kills$Infantry[KD_Kills$team == "AXIS"])

valuecards <- tibble(
  team = rep(c("ALLIES", "AXIS"), each = 2),
  metric = rep(c("KD", "ADJ KD"), 2),
  value = c(ALLIES_KD, ALLIES_KD_ADJ, AXIS_KD, AXIS_KD_ADJ)
)

# --- 5. Regular and Utility Kill Type Summaries ---

# Regular Kill Types Summary
RegKills <- KillTypesSummary %>%
  filter(WeaponType %in% weapon_types_regular) %>%
  mutate(WeaponType = factor(WeaponType, levels = weapon_types_regular)) %>% # Ensure factor levels are set
  complete(team, WeaponType, fill = list(total = 0)) # Explicit fill for complete

# Utility Kill Types Summary
UtilKills <- KillTypesSummary %>%
  filter(WeaponType %in% weapon_types_util) %>%
  mutate(WeaponType = factor(WeaponType, levels = weapon_types_util)) %>% # Ensure factor levels are set
  complete(team, WeaponType, fill = list(total = 0)) # Explicit fill for complete


# --- 6. Roster Based Assignments and Summaries ---

# Roster Data Extraction - Streamlined and more readable
extract_roster_names <- function(roster_csv, rows, col) {
  roster_csv %>%
    slice(rows) %>%
    pull(col) %>%
    na.omit() %>%
    as.list()
}

A1 <- extract_roster_names(roster_csv, 3:10, 5)
A2 <- extract_roster_names(roster_csv, 3:10, 9)
Flex <- extract_roster_names(roster_csv, 3:10, 13)
Def <- extract_roster_names(roster_csv, 19:31, 5)
MGs <- c(extract_roster_names(roster_csv, 11, 5),
         extract_roster_names(roster_csv, 11, 9),
         extract_roster_names(roster_csv, 11, 13)) %>% unlist() %>% na.omit() %>% as.list() # Handle list of lists

Sup <- c(extract_roster_names(roster_csv, 3:8, 17),
         extract_roster_names(roster_csv, 19:22, 9)) %>% unlist() %>% na.omit() %>% as.list() # Handle list of lists

Armor1 <- extract_roster_names(roster_csv, 19:21, 17)
Armor2 <- extract_roster_names(roster_csv, 22:24, 17)
Armor3 <- extract_roster_names(roster_csv, 25:27, 17)

SLs_lists <- list(
  extract_roster_names(roster_csv, 3:5, 5), #A1
  extract_roster_names(roster_csv, 3:5, 9), #A2
  extract_roster_names(roster_csv, 3:5, 13), #F
  extract_roster_names(roster_csv, 19, 9), # Wamo
  extract_roster_names(roster_csv, c(3,5,8), 17), # Recon/Sniper
  extract_roster_names(roster_csv, c(21,29:31), 5) # Def
)
SLs <- SLs_lists %>% unlist() %>% na.omit() %>% as.list() # Flatten the list of lists

GameInfo <- list(
  Map = roster_csv[[19, 13]],
  Side = roster_csv[[20, 13]],
  Opp = roster_csv[[21, 13]]
)

Specialists_lists <- list(
  Spotter = extract_roster_names(roster_csv, c(3, 5), 17),
  Sniper = extract_roster_names(roster_csv, c(4, 6), 17),
  Wamo = extract_roster_names(roster_csv, 19:22, 9),
  Arty = extract_roster_names(roster_csv, 8, 17),
  Commander = extract_roster_names(roster_csv, 19, 5)
)
Specialists <- map(Specialists_lists, ~unlist(.) %>% na.omit()) # Flatten all specialist lists


# Assignment and Role tagging
data_csv_assign <- left_join(data_csv2, Master_csv, join_by(`Player ID` == Steam64)) %>%
  select(-c(Weapons, `Death by Weapons`)) %>% # Remove redundant columns earlier
  left_join(DeathTypes, by = "Player ID") %>%
  mutate(
    Assignment = case_when(
      Roster_Name %in% A1 ~ "Assault 1",
      Roster_Name %in% A2 ~ "Assault 2",
      Roster_Name %in% Flex ~ "Flex",
      Roster_Name %in% MGs ~ "MG",
      Roster_Name %in% Def ~ "Defense",
      Roster_Name %in% Sup ~ "Support",
      Roster_Name %in% Armor1 ~ "Armor 1",
      Roster_Name %in% Armor2 ~ "Armor 2",
      Roster_Name %in% Armor3 ~ "Armor 3",
      TRUE ~ NA_character_ # Explicit NA character
    ),
    SL = ifelse(Roster_Name %in% SLs, "Y", "N"),
    Role = case_when(
      Roster_Name %in% Specialists[["Spotter"]] ~ "Spotter",
      Roster_Name %in% Specialists[["Sniper"]] ~ "Sniper",
      Roster_Name %in% Specialists[["Wamo"]] ~ "Wamo",
      Roster_Name %in% Specialists[["Arty"]] ~ "Arty",
      Roster_Name %in% Specialists[["Commander"]] ~ "Commander",
      !is.na(Assignment) ~ Assignment,
      TRUE ~ NA_character_ # Explicit NA character
    )
  ) %>%
  drop_na(Assignment) %>% # Keep drop_na at the end for clarity after assignment
  mutate(`Adj K/D` = calculate_kd(Kills, `Deaths (Inf)`),
         `% Shells` = round((`Deaths (Arty)` + `Deaths (Armor)`) / Deaths, 3))

# --- 7. Summary Tables ---

# Division Summary Table
DivSummary <- data_csv_assign %>%
  select(Assignment, Roster_Name, Role, SL, Kills, Deaths, `Combat Effectiveness`, `Support Points`, `K/D`, `Adj K/D`, `Kill(s) / minute`, `% Shells`) %>%
  rename(
    KPM = `Kill(s) / minute`,
    CE = `Combat Effectiveness`,
    SE = `Support Points`,
    Name = Roster_Name
  ) %>%
  arrange(desc(KPM))

# Assignment Summary Table
AssignmentSummary <- data_csv_assign %>%
  group_by(Role) %>%
  summarise(
    Players = n(),
    Kills = sum(Kills),
    Deaths = sum(Deaths),
    `Deaths by Inf` = sum(`Deaths (Inf)`),
    `Kills p/player` = calculate_kd(sum(Kills), n()),
    `OG KD` = round(mean(`K/D`), 2),
    `ADJ KD` = round(mean(`Adj K/D`), 2),
    `% Shells` = round(mean(`% Shells`, na.rm = TRUE), 2),
    CE = sum(`Combat Effectiveness`),
    SE = sum(`Support Points`),
    .groups = 'drop'
  ) %>%
  arrange(desc(Kills))

# CE/SE per player data
CE_SE_Summary <- AssignmentSummary %>%
  filter(!Role %in% c("Arty", "Commander")) %>%
  mutate(
    CE_Per_Player = round(CE / Players, 0),
    SE_Per_Player = round(SE / Players, 0)
  ) %>%
  select(Role, CE_Per_Player, SE_Per_Player) # Select relevant columns at the end