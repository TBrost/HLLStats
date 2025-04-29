#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(gt)
library(gtExtras)
source('HLL_Lists.R')
source('HLL_Functions.R')

data_csv <- read_csv("../GAMES/BOTN_VNGD_GAME_04-06-2025.csv",
                     col_types = cols(
                         `Player ID` = col_character(),
                     )) %>%
    select(-c(`Max kill streak`, `Death(s) / minute`, `Max death streak`, `Max TK streak`, `Death by TK`, `(aprox.) Longest life min.`,`(aprox.) Shortest life secs.`,`Nemesis`, `Victim`, `Death by TK Streak`))
Master_csv <- read_csv("../BOTN Master Roster/вит Rosters - MASTER ROSTER.csv",
                       col_types = cols(
                           `Steam64` = col_character(),
                       ),
                       col_select = c("Name:", "Steam64"),
                       skip =1) %>%
    drop_na(Steam64) %>%
    rename("Roster_Name" = "Name:")

roster_csv <- read_csv("../ROSTERS/BOTN_RONIN_VNGD_ROSTER_04-06-25.csv")

# Define server logic required to draw a histogram
function(input, output, session) {

    # --- 1. Nesting --- ####

    # Apply JSON conversion and nest weapon data
    data_csv_nested <- data_csv %>%
        mutate(
            Weapons = map(Weapons, json_to_nested_df),
            DeathsbyWeapons = map(`Death by Weapons`, json_to_nested_df)
        )

    # Death Types Calculation
    DeathTypes <- data_csv_nested %>% # Use data_csv_nested, not re-nested data_csv_nested2 if no changes were intended
        select(`Player ID`, DeathsbyWeapons) %>%
        unnest(cols = DeathsbyWeapons) %>%
        mutate(DeathType = case_when(
            Weapon %in% Arty ~ "Deaths (Arty)",
            Weapon %in% Armor ~ "Deaths (Armor)",
            TRUE ~ "Deaths (Inf)"
        )) %>%
        group_by(`Player ID`, DeathType) %>%
        summarise(total = sum(Count), .groups = 'drop') %>% # Explicitly drop groups for efficiency
        pivot_wider(names_from = DeathType, values_from = total, values_fill = 0) # Explicit fill


    # --- 2. Team/Info Assignment --- ####

    # gamedate <- reactive({
    #     file_info <- input$fileUpload$name
    #
    #     # Assuming the date is always in the format YYYYMMDD after the first underscore and before the hyphen
    #     parts <- strsplit(filename, "_")[[1]]
    #
    #     date_time_part <- parts[2]
    #     date_part_hyphen_split <- strsplit(date_time_part, "-")[[1]]
    #
    #
    #     date_str_yyyymmdd <- date_part_hyphen_split[1]
    #
    #     formatted_date <- format(as.Date(date_str_yyyymmdd, format = "%Y%m%d"), "%m/%d/%Y")
    #     formatted_date
    # })


    GameInfo <- list(
        Map = roster_csv[[19, 13]],
        Side = roster_csv[[20, 13]],
        Opp = roster_csv[[21, 13]]
    )

    teams <- if (GameInfo[["Side"]] == "Allies") {
        c("AXIS", "ALLIES")
    } else {
        c("ALLIES", "AXIS")
    }
    color_dat <-
        tibble(team = teams,
               col = c("firebrick4", "springgreen4"))
    team_colors <- rlang::set_names(color_dat$col, color_dat$team)

    # Determine team based on most frequent death weapon and kill weapon
    team_d <- data_csv_nested %>%
        select(`Player ID`, DeathsbyWeapons) %>%
        unnest(cols = DeathsbyWeapons) %>% # Efficiently unnest lists
        group_by(`Player ID`) %>%
        arrange(desc(Count), .by_group = TRUE) %>% # Arrange within each group
        slice(1) %>%
        ungroup() %>%
        mutate(team_d = factor(case_when(
            Weapon %in% US_WEAPONS ~ "AXIS", # Reversed logic as deaths are from enemy weapons
            Weapon %in% AXIS_WEAPONS ~ "ALLIES",
            TRUE ~ NA # Explicit NA character
        ))) %>%
        select(`Player ID`, team_d)

    team_k <- data_csv_nested %>%
        select(`Player ID`, Weapons) %>%
        unnest(cols = Weapons) %>% # Efficiently unnest lists
        group_by(`Player ID`) %>%
        arrange(desc(Count), .by_group = TRUE) %>% # Arrange within each group
        slice(1) %>%
        ungroup() %>%
        mutate(team_k = factor(case_when(
            Weapon %in% US_WEAPONS ~ "ALLIES",
            Weapon %in% AXIS_WEAPONS ~ "AXIS",
            TRUE ~ NA # Explicit NA character
        ))) %>%
        select(`Player ID`, team_k)

    team <- full_join(team_d, team_k, by = "Player ID") %>%
        mutate(team = factor(case_when(
            !is.na(team_d) & is.na(team_k) ~ team_d,
            is.na(team_d) & !is.na(team_k) ~ team_k,
            team_d == team_k ~ team_d,
            is.na(team_d) & is.na(team_k) ~ "ALLIES",
            TRUE ~ NA
        ))) %>%
        select(`Player ID`, team)

    data_csv2 <- left_join(data_csv, team, by = "Player ID")

    data_csv_nested <- data_csv2 %>%
        mutate(Weapons = map(Weapons, json_to_nested_df)) # Re-apply JSON conversion - seems redundant, check if needed

    # --- 3. Kill and Death Type Analysis --- ####


    # Kill Types Calculation
    KillTypes <- data_csv_nested %>% # Using data_csv_nested2 which is data_csv2 nested again.
        select(Name, Weapons, team) %>%
        unnest(cols=Weapons) %>%
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


    BulletAllKills <- data_csv2 %>%
        group_by(team) %>%
        drop_na(team) %>%
        summarise(total = sum(Kills))

    BulletAllKills_PerP <- data_csv2 %>%
        group_by(team) %>%
        drop_na(team) %>%
        summarise(total = round(sum(Kills)/n(),1))

    BulletInfRegularKills <- KillTypes %>%
        filter(WeaponType %in% weapon_types_regular) %>%
        group_by(team) %>%
        summarise(total = sum(Count))

    BulletInfUtilKills <- KillTypes %>%
        filter(WeaponType %in% weapon_types_util) %>%
        group_by(team) %>%
        summarise(total = sum(Count))

    BulletArmorKills <- KillTypes %>%
        filter(WeaponType == "Armor") %>%
        group_by(team) %>%
        summarise(total = sum(Count))

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

    # --- 4. Team and KD Calculations --- ####

    # Team Kill/Death Summary
    killteam_summary <- data_csv_nested %>%
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
        pivot_wider(names_from = WeaponCategory, values_from = Kills, values_fill = 0) %>% drop_na()


    AXIS_KD <- calculate_kd(rowSums(KD_Kills[KD_Kills$team == "AXIS", -1]), rowSums(KD_Kills[KD_Kills$team == "ALLIES", -1]))
    ALLIES_KD <- calculate_kd(rowSums(KD_Kills[KD_Kills$team == "ALLIES", -1]), rowSums(KD_Kills[KD_Kills$team == "AXIS", -1]))
    AXIS_KD_ADJ <- calculate_kd(KD_Kills$Infantry[KD_Kills$team == "AXIS"], KD_Kills$Infantry[KD_Kills$team == "ALLIES"])
    ALLIES_KD_ADJ <- calculate_kd(KD_Kills$Infantry[KD_Kills$team == "ALLIES"], KD_Kills$Infantry[KD_Kills$team == "AXIS"])




    # --- 5. Regular and Utility Kill Type Summaries --- ####

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


    # --- 6. Roster Based Assignments and Summaries --- ####

    # Roster Data Extraction - Streamlined and more readable


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
        extract_roster_names(roster_csv, c(21,29:31), 5), # Def
        extract_roster_names(roster_csv, c(19,22,25), 17) # Armor
    )
    SLs <- SLs_lists %>% unlist() %>% na.omit() %>% as.list() # Flatten the list of lists

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
        #drop_na(Assignment) %>% # Keep drop_na at the end for clarity after assignment
        mutate(`Adj K/D` = ifelse(`Deaths (Inf)`!=0, round(Kills/`Deaths (Inf)`,2), `K/D`),
               `% Shells` = round((`Deaths (Arty)` + `Deaths (Armor)`) / Deaths, 3))

    # --- 7. Summary Tables --- ####

    # Division Summary Table
    DivSummary <- data_csv_assign %>%
        select(Assignment, Roster_Name, Role, SL, Kills, Deaths, `Combat Effectiveness`, `Support Points`, `K/D`, `Adj K/D`, `Kill(s) / minute`, `% Shells`) %>%
        separate(col = Roster_Name,      # The column to split
            into = c("Clan", "Name"), # Names for the new columns
            sep = "\\|",           # The delimiter (escaped '|')
            remove = TRUE         # Whether to remove the original column
        ) %>%
        rename(
            KPM = `Kill(s) / minute`,
            CE = `Combat Effectiveness`,
            SE = `Support Points`
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

    ## Div Tables ####

    ### Assault 1 ####

    A1Sum <- DivTable(DivSummary, c("Assault 1"))

    ### Assault 2 ####

    A2Sum <- DivTable(DivSummary, c("Assault 2"))

    ### Flex ####

    FlexSum <- DivTable(DivSummary, c("Flex"))

    ### MG's ####

    MGSum <- DivTable(DivSummary, c("MG"))

    ### Support ####

    SupportSum <- DivTable(DivSummary,  c("Support"))

    ### Defense ####

    DefSum <- DivTable(DivSummary, c("Defense"))

    ### Armor ####

    ArmorSum <- DivTable(DivSummary, c("Armor 1","Armor 2","Armor 3"))


    ### Div Sum ####
    gt_table_final <- AssignmentSummary  %>%
        filter(!Role %in% c("Armor 1", "Armor 2", "Armor 3")) %>%
        gt() %>%
        # tab_header(
        #     title = "Helios Assignment Performance"
        # ) %>%
        cols_align(
            align = "left",
            columns = Role
        ) %>%
        cols_align(
            align = "center",
            columns = -Role
        ) %>%
        gt_plt_bar(column = Players, color = "skyblue2",keep_column = FALSE, width = 20, scale_type =  "number") %>%
        #cols_move(columns = Players, after = Role
        #) %>%
        fmt_number(
            columns = c(`OG KD`, `ADJ KD`),
            decimals = 1
        ) %>%
        fmt_integer(
            columns = c(Kills, Deaths, `Deaths by Inf`, `Kills p/player`, CE, SE)
        ) %>%
        fmt_percent(
            columns = `% Shells`, decimals = 0
        ) %>%
        gt_color_box(columns = `Kills p/player`, domain = c(0,max(AssignmentSummary$`Kills p/player`)),
                     palette = c("lightcoral", "lightgreen", "green3")) %>%
        data_color(columns = Kills, palette = c("forestgreen","forestgreen")) %>%
        data_color(columns = Deaths, palette = c("firebrick3","firebrick3")) %>%
        data_color(columns = `ADJ KD`, palette = c("red2","green3")) %>%
        data_color(columns = `% Shells`, palette = c("lightgreen","lightcoral"), domain = range(AssignmentSummary$`% Shells`, na.rm = T)) %>%
        cols_width(
            Role ~ px(80),
            Players ~px(80),
            Kills ~ px(40),
            Deaths ~ px(50),
            `Deaths by Inf` ~ px(80),
            `Kills p/player` ~ px(80),
            CE ~ px(40),
            SE ~ px(40),
            `OG KD` ~ px(60),
            `ADJ KD` ~ px(60),
            `% Shells` ~ px(60)
        ) %>%
        tab_options(
            table.font.size = 12
        )





    # Primary Metrics ####

    ## Game Info ####
    output$game_info_card <- renderUI({
        Date <- "3/23/2025" # Static Date (replace with your actual date object)
        TeamName <- "BOTN" # Static Team Name
        outcome_text <- input$game_outcome
        team_side <- GameInfo[["Side"]]
        opponent_team <- GameInfo[["Opp"]]
        game_map <- GameInfo[["Map"]]
        game_date <- Date

        # Determine opponent's side dynamically
        opponent_side <- if (team_side == "Axis") "Allies" else "Axis"

        # Determine Win/Loss color
        outcome_parts <- strsplit(outcome_text, " - ")[[1]]
        outcome_status <- outcome_parts[1]
        outcome_color <- if (startsWith(outcome_status, "WIN")) "green" else "red"

        line1 <- tags$p(
            style = "color: white; font-size: 16px; text-align: center; margin-bottom: 5px; line-height: 1;",
            HTML(paste0("<b>",TeamName, "</b> (", team_side, ") vs <b>", opponent_team, "</b> (", opponent_side, ")")) # Bold opponent name
        )
        line2 <- tags$p(
            style = "color: white; font-size: 12px; text-align: center; margin-bottom: 5px; line-height: 1;",
            game_date
        )
        line3 <- tags$p(
            style = "color: white; font-size: 12px; text-align: center; margin-bottom: 5px; line-height: 1;",
            game_map
        )
        line4 <- tags$p(
            style = paste0("color: ", outcome_color, "; font-weight: bold; font-size: 14px; text-align: center; margin-bottom: 0px; line-height: 1;"),
            outcome_text # Display full outcome text
        )

        tags$div(line1, line2, line3, line4)
    })

    ### BOTN KD ####

    output$BOTNKD <- renderUI({
        value <- req(AXIS_KD)
        team <- "BOTN"
        red <- 255 * (1 - (value / 1.5)) # Directly use value/2
        green <- 255 * (value / 1.5)     # Directly use value/2
        text_color <- sprintf("rgb(%s, %s, 0)", red, green)

        line1 <- tags$p(style = paste0("color: ", text_color, "; font-size: 32px; text-align: center; margin-bottom: 3px; margin-top: 0px;"), value)
        line2 <- tags$p(style = "color: white; font-weight: bold; font-size: 12px; text-align: center; margin-bottom: 0px;", paste0(team," KD"))

        tags$div(line1, line2)
    })

    ### Opp KD ####
    output$opponentKD <- renderUI({
        value <- req(ALLIES_KD)
        team <- "PV"
        red <- 255 * (1 - (value / 1.5)) # Directly use value/2
        green <- 255 * (value / 1.5)     # Directly use value/2
        text_color <- sprintf("rgb(%s, %s, 0)", red, green)

        line1 <- tags$p(style = paste0("color: ", text_color, "; font-size: 32px; text-align: center; margin-bottom: 3px; margin-top: 0px;"), value)
        line2 <- tags$p(style = "color: white; font-weight: bold; font-size: 12px; text-align: center; margin-bottom: 0px;", paste0(team," KD"))

        tags$div(line1, line2)
    })

    ## Performance Comparison ####

    ### Inf Regular ####

    output$InfRegular <- renderPlot(
        ggplot(RegKills, aes(x = total, y = fct_reorder(WeaponType, total) , fill = team)) +
        geom_col(position = 'dodge') +
        geom_text(aes(label = total, group=team),
                  position = position_dodge(width = 0.9), vjust = 0.5, hjust=-0.2, size = 4, color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        scale_x_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_line(color = "gray20", linetype = "dotted"), # Dotted Y-Grid
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_line(color = "gray100", linetype = "dotted"),,
            axis.text = element_text(color = "white"),  # White Axis Text
            axis.title = element_text(color = "white"), #White axis labels
            plot.title = element_text(color = "white", hjust = 0.5, size =15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 30, size = 11),#Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
        ) +
        coord_cartesian(xlim = c(0,max(RegKills$total)+100)) +
        labs(title = "Infantry Regular Kills",
             x = NULL,
             y = NULL)
    )

    ### Inf Utility ####

    output$InfUtility <- renderPlot(
        ggplot(UtilKills, aes(x = total, y = fct_reorder(WeaponType, total) , fill = team)) +
        geom_col(position = 'dodge') +
        geom_text(aes(label = total, group=team),
                  position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.1, size = 4, color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_line(color = "gray20", linetype = "dotted"), # Dotted Y-Grid
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_line(color = "gray100", linetype = "dotted"),,
            axis.text = element_text(color = "white"),  # White Axis Text
            axis.title = element_text(color = "white"), #White axis labels
            plot.title = element_text(color = "white", hjust = 0.5, size =15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 30, size = 11),#Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Infantry Utility Kills",
             x = NULL,
             y = NULL)
    )

    ### Armor ####

    output$ArmorKills <- renderPlot(
        ggplot(ArmorKills, aes(x = total, y = fct_reorder(Name, total) , fill = team)) +
        geom_col(position = 'dodge') +
        geom_text(aes(label = total, group=team),
                  position = position_dodge(width = 0.9), vjust = 0.5, hjust=1.2, size = 4, color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_line(color = "gray20", linetype = "dotted"), # Dotted Y-Grid
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_line(color = "gray100", linetype = "dotted"),,
            axis.text = element_text(color = "white"),  # White Axis Text
            axis.title = element_text(color = "white"), #White axis labels
            plot.title = element_text(color = "white", hjust = 0.5, size =15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12), #Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Armor Kills",
             x = NULL,
             y = NULL)
    )

    ## CE/SE by Div ####

    ### CE ####

    output$CEByPlayer <- renderPlot(
        ggplot(CE_SE_Summary, aes(y = fct_reorder(Role, CE_Per_Player), x = CE_Per_Player, fill = Role)) +
        geom_col() +
        geom_text(aes(label = CE_Per_Player),
                  position = position_dodge(width = 0.9), vjust = 0.5, hjust= 1.5, size = 4, color="white") + #added color white to make it see easier
        scale_fill_manual(values = c("Armor 1" = "gray70",
                                     "Armor 2" = "gray70",
                                     "Armor 3" = "gray70",
                                     "Sniper" = "brown2",
                                     "Spotter" = "brown2",
                                     "Assault 1" = "forestgreen",
                                     "Assault 2" = "goldenrod",
                                     "Flex" = "darkorchid",
                                     "Defense" = "deepskyblue3",
                                     "MG" = "darkorchid1",
                                     "Wamo" = "darkorange2")) +
        #scale_y_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_line(color = "gray20", linetype = "dotted"), # Dotted Y-Grid
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_line(color = "gray100", linetype = "dotted"),
            axis.text = element_text(color = "white"),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size =15), # White Title, Centered
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            axis.text.x = element_text(angle = 45, hjust = 1), #Rotate X Axis
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            legend.title = element_blank()
        ) +
        labs(title = "CE Per Player",
             x = NULL,
             y = NULL)
    )

    ### SE ####

    output$SEByPlayer <- renderPlot(
        ggplot(CE_SE_Summary, aes(y = fct_reorder(Role, SE_Per_Player), x = SE_Per_Player, fill = Role)) +
        geom_col() +
        geom_text(aes(label = SE_Per_Player),
                  position = position_dodge(width = 0.9), vjust = 0.5, hjust= 1.5, size = 4, color="white") + #added color white to make it see easier
        scale_fill_manual(values = c("Armor 1" = "gray70",
                                     "Armor 2" = "gray70",
                                     "Armor 3" = "gray70",
                                     "Sniper" = "brown2",
                                     "Spotter" = "brown2",
                                     "Assault 1" = "forestgreen",
                                     "Assault 2" = "goldenrod",
                                     "Flex" = "darkorchid",
                                     "Defense" = "deepskyblue3",
                                     "MG" = "darkorchid1",
                                     "Wamo" = "darkorange2")) +
        #scale_y_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_line(color = "gray20", linetype = "dotted"), # Dotted Y-Grid
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_line(color = "gray100", linetype = "dotted"),
            axis.text = element_text(color = "white"),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size =15), # White Title, Centered
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            axis.text.x = element_text(angle = 45, hjust = 1), #Rotate X Axis
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            legend.title = element_blank()
        ) +
        labs(title = "SE Per Player",
             x = NULL,
             y = NULL)
    )

    ## Bullet Charts ####

    ### Total Kills ####

    output$TotalKillsByTeam <- renderPlot(
        ggplot(BulletAllKills, aes(x = total, y=factor(1) , fill = team)) +
        geom_col() +
        geom_text(aes(label = total, group=team), vjust = 0.5, hjust=3, size = 5, position="stack", color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size = 15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent", color = NA), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            legend.position = "top",
            legend.title = element_blank(), # Remove legend title (if any)
            ) +
        labs(title = "Total Kills by Team",
             x = NULL,
             y = NULL)
    )

    ### Kills Per Member ####

    output$KillsPerMemberByTeam <- renderPlot(
        ggplot(BulletAllKills_PerP, aes(x = total, y=factor(1) , fill = team)) +
        geom_col() +
        geom_text(aes(label = total, group=team), vjust = 0.5, hjust=3, size = 5, position="stack", color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        #scale_x_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        # geom_vline(aes(xintercept = sum(BulletAllKills$total)/2), # Map yintercept to the 50% value
        #            color = "white",
        #            linetype = "dashed", # Dotted line
        #            size = 1)+
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_blank(),#White axis labels
            axis.text = element_blank(),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size = 15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            #axis.text.x = element_text(color = "White", angle = 45, hjust = 1), #Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Kills per Player",
             x = NULL,
             y = NULL)
        )

    ### Total Inf Regular ####

    output$TotalInfantryRegularKills <- renderPlot(
        ggplot(BulletInfRegularKills, aes(x = total, y=factor(1) , fill = team)) +
        geom_col() +
        geom_text(aes(label = total, group=team), vjust = 0.5, hjust=3, size = 5, position="stack", color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        #scale_x_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        geom_vline(aes(xintercept = sum(BulletInfRegularKills$total)/2), # Map yintercept to the 50% value
                   color = "white",
                   linetype = "dashed", # Dotted line
                   size = 1)+
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size = 15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            #axis.text.x = element_text(color = "White", angle = 45, hjust = 1), #Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Infantry Regular Kills",
             x = NULL,
             y = NULL)
    )

    ### Total Inf Utility ####

    output$TotalInfantryUtilityKills <- renderPlot(
        ggplot(BulletInfUtilKills, aes(x = total, y=factor(1) , fill = team)) +
        geom_col() +
        geom_text(aes(label = total, group=team), vjust = 0.5, hjust=4, size = 5, position="stack", color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        #(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        geom_vline(aes(xintercept = sum(BulletInfUtilKills$total)/2), # Map yintercept to the 50% value
                   color = "white",
                   linetype = "dashed", # Dotted line
                   size = 1)+
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size = 15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            #axis.text.x = element_text(color = "White", angle = 45, hjust = 1), #Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Infantry Utility Kills",
             x = NULL,
             y = NULL)
    )

    ### Total Armor ####

    output$TotalArmorKills <- renderPlot(
        ggplot(BulletArmorKills, aes(x = total, y=factor(1) , fill = team)) +
        geom_col() +
        geom_text(aes(label = total, group=team), vjust = 0.5, hjust=3.5, size = 5, position="stack", color="white") + #added color white to make it see easier
        scale_fill_manual(values = team_colors) +
        #scale_x_continuous(labels = function(x) paste0(x/1000, "K")) + # Format y-axis
        geom_vline(aes(xintercept = sum(BulletArmorKills$total)/2), # Map yintercept to the 50% value
                   color = "white",
                   linetype = "dashed", # Dotted line
                   size = 1)+
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "#2d2d2d", color = NA), # Black Background
            plot.background = element_rect(fill = "#2d2d2d", color = NA), #Black Plot Backround
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),  # Remove Minor Gridlines
            panel.grid.major.x = element_blank(),
            axis.text = element_blank(),  # White Axis Text
            plot.title = element_text(color = "white", hjust = 0.5, size = 15), # White Title, Centered
            legend.text = element_text(color = "white"), #White Legend text
            legend.background = element_rect(fill = "transparent"), # Transparent Legend Background,
            legend.key = element_rect(fill = "transparent"),
            #axis.text.x = element_text(color = "White", angle = 45, hjust = 1), #Rotate X Axis
            legend.position = "none",
            legend.title = element_blank()
            ) +
        labs(title = "Armor Kills",
             x = NULL,
             y = NULL)
    )

    ## Div Summary ####

    output$divisionPerformance <- render_gt(
        gt_table_final %>%
            opt_css(
                css = "
            body { /* Or you can target .gt_table for more specific styling */
              background-color: #2d2d2d;
              color: white; /* Default text color to white */
            }
            .gt_table {
              background-color: #2d2d2d; /* Ensure table background is gray20 */
              color: white; /* Ensure table text is white */
              border: none; /* Optional: Remove default table border if you want */
            }
            .gt_table th { /* Style table header cells */
              color: white; /* Ensure header text is white */
              background-color: #2d2d2d; /* Ensure header background is gray20 */
              border-bottom: 1px solid white; /* Optional: White line under headers */
            }
            .gt_table td { /* Style table data cells */
              color: white; /* Ensure data text is white */
              background-color: #2d2d2d; /* Ensure data background is gray20 */
              border: none; /* Optional: Remove cell borders if you want */
            }
            .gt_table tfoot { /* Style table footer if you have one */
              color: white;
              background-color: #2d2d2d;
            }
            .gt_table caption { /* Style table caption if you have one */
              color: white;
              background-color: #2d2d2d;
            }
        "
            )
    )


    # Div Metrics

    ## Assault 1

    output$A1Performance <- render_gt(
        A1Sum
    )

    ## Assault 2

    output$A2Performance <- render_gt(
        A2Sum
    )

    ## Flex

    output$FlexPerformance <- render_gt(
        FlexSum
    )

    ## MG's

    output$MGPerformance <- render_gt(
        MGSum
    )

    ## Support

    output$SupportPerformance <- render_gt(
        SupportSum
    )

    ## Defense

    output$DefPerformance <- render_gt(
        DefSum
    )

    ## Armor

    output$ArmorPerformance <- render_gt(
        ArmorSum
    )


}
