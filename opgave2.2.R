library(RMariaDB)
library(DBI)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

con <- dbConnect(
  MariaDB(),
  host     = "www.talmedos.com",
  port     = 3306,
  user     = "dalremote",
  password = "OttoRehagel123456789Long2026!",
  dbname   = "superliga2"
)

s1 <- 189918
s2 <- 191611

##########################################################
###################### DATA (SHOTS) ######################
##########################################################

# 1) Hent skud for de to sæsoner (Superliga)
shots_24_25 <- dbGetQuery(con, "
SELECT s.*, m.SEASON_WYID, m.COMPETITION_WYID, m.MATCH_WYID
FROM wyscout_matchevents_shots AS s
JOIN wyscout_matches AS m
  ON m.MATCH_WYID = s.MATCH_WYID
WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 189918;
") %>% distinct(EVENT_WYID, .keep_all = TRUE)

shots_25_26 <- dbGetQuery(con, "
SELECT s.*, st.PRIMARYTYPE, m.SEASON_WYID, m.COMPETITION_WYID, m.MATCH_WYID
FROM wyscout_matchevents_shots AS s
JOIN wyscout_matches AS m
  ON m.MATCH_WYID = s.MATCH_WYID
LEFT JOIN wyscout_matchevents_secondarytype AS st
  ON st.EVENT_WYID = s.EVENT_WYID
WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 191611;
") %>% distinct(EVENT_WYID, .keep_all = TRUE)

# 2) Saml sæsonerne
shots_season_24_25_26 <- bind_rows(shots_24_25, shots_25_26)

# 3) Fjern "outliers": corn, free, pena  -> HER opstår 6885
shots_season_24_25_26_cpf <- shots_season_24_25_26 %>%
  filter(!PRIMARYTYPE %in% c("corn", "free", "pena"))

# 4) Dokumentér tallet (før/efter + fjernet)
print(
  shots_season_24_25_26 %>%
    summarise(
      n_before = n(),
      n_after  = sum(!PRIMARYTYPE %in% c("corn","free","pena")),
      removed  = sum(PRIMARYTYPE %in% c("corn","free","pena"))
    )
)

# 5) total_goal (kombiner SHOTISGOAL + "goal" i SHOTBODYPART)
shots_season_24_25_26_cpf <- shots_season_24_25_26_cpf %>%
  mutate(
    goal_from_shotisgoal = as.integer(SHOTISGOAL == 1),
    goal_from_bodypart   = as.integer(str_detect(tolower(as.character(SHOTBODYPART)), "goal")),
    total_goal           = as.integer(goal_from_shotisgoal == 1 | goal_from_bodypart == 1)
  ) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID))

##########################################################
################## COMMON (TEAM/PLAYER/MINUTE) ###########
##########################################################

# Common: TEAM_WYID, PLAYER_WYID, MINUTE for shot-events
common_shots_min <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, TEAM_WYID, PLAYER_WYID, MINUTE
  FROM wyscout_matchevents_common
  WHERE PRIMARYTYPE = 'shot'
    AND SEASON_WYID IN (%s, %s);
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

shots_master <- shots_season_24_25_26_cpf %>%
  left_join(common_shots_min, by = "EVENT_WYID")

##########################################################
###################### COORDS (common_coords) ############
##########################################################

common_coords <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, LOCATIONX, LOCATIONY
  FROM wyscout_matchevents_common
  WHERE PRIMARYTYPE = 'shot'
    AND SEASON_WYID IN (%s, %s);
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

##########################################################
###################### TEAM NAMES ########################
##########################################################

team_names <- dbGetQuery(con, sprintf("
  SELECT DISTINCT TEAM_WYID, TEAMNAME
  FROM wyscout_teams
  WHERE COMPETITION_WYID = 335
    AND SEASON_WYID IN (%s, %s);
", s1, s2)) %>%
  distinct(TEAM_WYID, .keep_all = TRUE)

##########################################################
###################### PLAYER ROLES ######################
##########################################################

# Hent ROLENAME (og navn) fra wyscout_players for de spillere vi faktisk har i shots_master
player_ids <- unique(na.omit(shots_master$PLAYER_WYID))
player_ids_txt <- paste(player_ids, collapse = ",")

players_role <- dbGetQuery(con, sprintf("
  SELECT PLAYER_WYID, SHORTNAME, FIRSTNAME, LASTNAME, ROLENAME
  FROM wyscout_players
  WHERE PLAYER_WYID IN (%s)
    AND COMPETITION_WYID = 335
    AND SEASON_WYID IN (%s, %s);
", player_ids_txt, s1, s2)) %>%
  mutate(player_name = ifelse(!is.na(SHORTNAME) & SHORTNAME != "", SHORTNAME,
                              paste(FIRSTNAME, LASTNAME))) %>%
  # undgå many-to-many: én række pr spiller (vælg første ikke-NA rolename)
  arrange(PLAYER_WYID) %>%
  group_by(PLAYER_WYID) %>%
  summarise(
    player_name = first(na.omit(player_name)),
    ROLENAME    = first(na.omit(ROLENAME)),
    .groups = "drop"
  )

##########################################################
######################### PLOT 1: MÅL vs IKKE MÅL ########
##########################################################

goal_share <- shots_season_24_25_26_cpf %>%
  mutate(outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")) %>%
  count(outcome) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(goal_share, aes(x = outcome, y = pct, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title = "Mål vs ikke mål (procent)",
    x = NULL,
    y = "Andel af skud (%)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

##########################################################
######## PLOT 2: Top 5 hold lavest gennemsnits-distance ###
##########################################################

team_mean_dist <- shots_master %>%
  mutate(season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26")) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    distance_m = sqrt((105 - x_m)^2 + (34 - y_m)^2)
  ) %>%
  group_by(season, TEAM_WYID) %>%
  summarise(
    shots = n(),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 150) %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(season) %>%
  slice_min(mean_distance_m, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(mean_distance_m = round(mean_distance_m, 2))

team_mean_dist_plot <- team_mean_dist %>%
  mutate(season = factor(season, levels = c("2024/25","2025/26"))) %>%
  group_by(season) %>%
  arrange(mean_distance_m, .by_group = TRUE) %>%  # lavest "øverst" efter coord_flip? -> vi styrer med levels nedenfor
  mutate(team_season = factor(paste0(TEAMNAME, " (", season, ")"),
                              levels = paste0(TEAMNAME, " (", season, ")"))) %>%
  ungroup()

ggplot(team_mean_dist_plot, aes(x = team_season, y = mean_distance_m, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = mean_distance_m), hjust = -0.15, size = 4) +
  coord_flip() +
  facet_wrap(~ season, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub(" \\(.*\\)$", "", x)) +
  labs(
    title = "Randers har begge sæsoner den lavest gennemsnitlig afstand til mål (min. 150 skud)",
    x = NULL,
    y = "Gennemsnitlig afstand til mål (meter)",
    fill = "Sæson",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13)
############# NU højst ##############
team_mean_dist <- shots_master %>%
  mutate(season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26")) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    distance_m = sqrt((105 - x_m)^2 + (34 - y_m)^2)
  ) %>%
  group_by(season, TEAM_WYID) %>%
  summarise(
    shots = n(),
    mean_distance_m = mean(distance_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 150) %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(season) %>%
  slice_max(mean_distance_m, n = 5, with_ties = FALSE) %>%  # <-- ændringen
  ungroup() %>%
  mutate(mean_distance_m = round(mean_distance_m, 2))

team_mean_dist_plot <- team_mean_dist %>%
  mutate(season = factor(season, levels = c("2024/25","2025/26"))) %>%
  group_by(season) %>%
  arrange(desc(mean_distance_m), .by_group = TRUE) %>%  # <-- sorter høj->lav
  mutate(team_season = factor(
    paste0(TEAMNAME, " (", season, ")"),
    levels = paste0(TEAMNAME, " (", season, ")")
  )) %>%
  ungroup()

ggplot(team_mean_dist_plot, aes(x = team_season, y = mean_distance_m, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = mean_distance_m), hjust = -0.15, size = 4) +
  coord_flip() +
  facet_wrap(~ season, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub(" \\(.*\\)$", "", x)) +
  labs(
    title = "Velje med højest gennemsnitlig afstand til mål (min. 150 skud)",
    x = NULL,
    y = "Gennemsnitlig afstand til mål (meter)",
    fill = "Sæson",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13)
##########################################################
######## PLOT 3: Top 5 hold højeste gennemsnits-vinkel ####
##########################################################

team_mean_angle <- shots_master %>%
  mutate(season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26")) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    
    goal_x_m = 105,
    left_post_y_m  = 34 - 7.32/2,
    right_post_y_m = 34 + 7.32/2,
    
    a1 = atan2(left_post_y_m  - y_m, goal_x_m - x_m),
    a2 = atan2(right_post_y_m - y_m, goal_x_m - x_m),
    ang = abs(a2 - a1),
    ang = ifelse(ang > pi, 2*pi - ang, ang),
    angle_deg = ang * 180 / pi
  ) %>%
  filter(is.finite(angle_deg), angle_deg >= 0, angle_deg <= 180) %>%
  group_by(season, TEAM_WYID) %>%
  summarise(
    shots = n(),
    mean_angle = mean(angle_deg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 150) %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(season) %>%
  slice_max(mean_angle, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(mean_angle = round(mean_angle, 1))

team_mean_angle_plot <- team_mean_angle %>%
  mutate(season = factor(season, levels = c("2024/25","2025/26"))) %>%
  group_by(season) %>%
  arrange(mean_angle, .by_group = TRUE) %>%  # højeste nederst før flip -> levels styrer
  mutate(team_season = factor(paste0(TEAMNAME, " (", season, ")"),
                              levels = paste0(TEAMNAME, " (", season, ")"))) %>%
  ungroup()

ggplot(team_mean_angle_plot, aes(x = team_season, y = mean_angle, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = mean_angle), hjust = -0.15, size = 4) +
  coord_flip() +
  facet_wrap(~ season, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub(" \\(.*\\)$", "", x)) +
  labs(
    title = "AGF og Viborg er de hold med højest gennemsnitlig vinkel til mål i de to sæsoner (min. 150 skud)",
    x = NULL,
    y = "Gennemsnitlig vinkel (grader)",
    fill = "Sæson",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13)
################## lavest 
team_mean_angle <- shots_master %>%
  mutate(season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26")) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    
    goal_x_m = 105,
    left_post_y_m  = 34 - 7.32/2,
    right_post_y_m = 34 + 7.32/2,
    
    a1 = atan2(left_post_y_m  - y_m, goal_x_m - x_m),
    a2 = atan2(right_post_y_m - y_m, goal_x_m - x_m),
    ang = abs(a2 - a1),
    ang = ifelse(ang > pi, 2*pi - ang, ang),
    angle_deg = ang * 180 / pi
  ) %>%
  filter(is.finite(angle_deg), angle_deg >= 0, angle_deg <= 180) %>%
  group_by(season, TEAM_WYID) %>%
  summarise(
    shots = n(),
    mean_angle = mean(angle_deg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 150) %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(season) %>%
  slice_min(mean_angle, n = 5, with_ties = FALSE) %>%   # <-- ændringen
  ungroup() %>%
  mutate(mean_angle = round(mean_angle, 1))

team_mean_angle_plot <- team_mean_angle %>%
  mutate(season = factor(season, levels = c("2024/25","2025/26"))) %>%
  group_by(season) %>%
  arrange(mean_angle, .by_group = TRUE) %>%   # lavest øverst efter flip
  mutate(team_season = factor(
    paste0(TEAMNAME, " (", season, ")"),
    levels = paste0(TEAMNAME, " (", season, ")")
  )) %>%
  ungroup()

ggplot(team_mean_angle_plot, aes(x = team_season, y = mean_angle, fill = season)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = mean_angle), hjust = -0.15, size = 4) +
  coord_flip() +
  facet_wrap(~ season, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub(" \\(.*\\)$", "", x)) +
  labs(
    title = "Nordsjælland og Silkeborg 5 med lavest gennemsnitlig vinkel til mål i de to sæsoner (min. 150 skud)",
    x = NULL,
    y = "Gennemsnitlig vinkel (grader)",
    fill = "Sæson",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13)



##########################################################
################ OPGAVE 3: skudlængde mål vs ikke-mål #####
##########################################################

length_goal_vs_nongoal <- shots_master %>%
  mutate(season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26")) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    length_m = sqrt((105 - x_m)^2 + (34 - y_m)^2),
    outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")
  ) %>%
  group_by(season, outcome) %>%
  summarise(
    mean_length_m = mean(length_m, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(length_goal_vs_nongoal, aes(x = outcome, y = mean_length_m, fill = outcome)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = round(mean_length_m, 1)), vjust = -0.3, size = 4) +
  facet_wrap(~ season) +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title = "Skud tættere på mål resulterer oftere i scoringer",
    x = NULL,
    y = "Gennemsnitlig skudlængde (meter)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

##########################################################
################ OPGAVE 4: andel af mål pr position #######
##########################################################

shots_by_role <- shots_master %>%
  left_join(players_role, by = "PLAYER_WYID") %>%
  mutate(
    season = ifelse(SEASON_WYID == s1, "2024/25", "2025/26"),
    role_group = case_when(
      # simple og robust: brug kun Wyscouts hoved-roller
      ROLENAME == "Defender"    ~ "Forsvar",
      ROLENAME == "Midfielder"  ~ "Midtbane",
      ROLENAME == "Forward"     ~ "Angreb",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(role_group)) %>%
  group_by(season, role_group) %>%
  summarise(
    goals = sum(total_goal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(season) %>%
  mutate(goal_share = goals / sum(goals) * 100) %>%
  ungroup()

ggplot(shots_by_role, aes(x = role_group, y = goal_share, fill = role_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(goal_share, 1), "%")),
            vjust = -0.4, size = 4) +
  facet_wrap(~ season) +
  labs(
    title = "Målfordelingen er domineret af angrebsspillere - stabilt på tværs af sæsoner",
    x = NULL,
    y = "Andel af mål (%)",
    fill = "Position",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

####################### 2024/25: Top 10 målscorere (mål vs xG)

shots_24_25 <- dbGetQuery(con, sprintf("
SELECT s.*, m.SEASON_WYID, m.COMPETITION_WYID, m.MATCH_WYID
FROM wyscout_matchevents_shots AS s
JOIN wyscout_matches AS m
  ON m.MATCH_WYID = s.MATCH_WYID
WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = %s;
", s1)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID))

# Fjern corn/free/pena + total_goal
shots_24_25_cpf <- shots_24_25 %>%
  filter(!PRIMARYTYPE %in% c("corn", "free", "pena")) %>%
  mutate(
    goal_from_shotisgoal = as.integer(SHOTISGOAL == 1),
    goal_from_bodypart   = as.integer(str_detect(tolower(as.character(SHOTBODYPART)), "goal")),
    total_goal           = as.integer(goal_from_shotisgoal == 1 | goal_from_bodypart == 1)
  )

# Common (TEAM/PLAYER/MINUTE) kun for s1
common_shots_min_s1 <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, TEAM_WYID, PLAYER_WYID, MINUTE
  FROM wyscout_matchevents_common
  WHERE PRIMARYTYPE = 'shot'
    AND SEASON_WYID = %s;
", s1)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

shots_master_s1 <- shots_24_25_cpf %>%
  left_join(common_shots_min_s1, by = "EVENT_WYID")

top10_goals_s1 <- shots_master_s1 %>%
  left_join(players_role, by = "PLAYER_WYID") %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(PLAYER_WYID, player_name, TEAMNAME) %>%
  summarise(
    goals = sum(total_goal, na.rm = TRUE),
    xg    = sum(SHOTXG, na.rm = TRUE),
    shots = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(goals)) %>%
  slice_head(n = 10) %>%
  mutate(
    diff = goals - xg,
    over_under = ifelse(diff > 0, "Overperformer", "Underperformer")
  )

ggplot(top10_goals_s1, aes(x = reorder(player_name, goals))) +
  geom_col(aes(y = goals, fill = TEAMNAME), width = 0.6) +
  geom_point(aes(y = xg), color = "black", size = 3) +
  geom_text(aes(y = goals, label = goals), hjust = -0.2, size = 4) +
  geom_text(aes(y = xg, label = round(xg, 1)), nudge_y = 0.2, size = 3.5, color = "black") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    limits = c(-0.5, max(top10_goals_s1$goals, na.rm = TRUE) * 1.15),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 målscorere i 2024/25 (mål vs xG)",
    subtitle = "Søjler viser mål - prikker viser xG",
    x = NULL,
    y = "Antal mål / xG",
    fill = "Hold",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.margin = margin(10, 30, 10, 10))

####################### 2025/26: Top 10 målscorere (til og med 08/12/2025)

cutoff_date <- as.Date("2025-12-08")

shots_25_26_upto <- dbGetQuery(con, sprintf("
SELECT s.*, st.PRIMARYTYPE,
       m.SEASON_WYID, m.COMPETITION_WYID, m.MATCH_WYID,
       m.DATEUTC AS match_date
FROM wyscout_matchevents_shots AS s
JOIN wyscout_matches AS m
  ON m.MATCH_WYID = s.MATCH_WYID
LEFT JOIN wyscout_matchevents_secondarytype AS st
  ON st.EVENT_WYID = s.EVENT_WYID
WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = %s;
", s2)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE) %>%
  mutate(
    match_date = as.Date(match_date),
    EVENT_WYID = as.character(EVENT_WYID)
  ) %>%
  filter(match_date <= cutoff_date)

# Fjern corn/free/pena + total_goal
shots_25_26_upto_cpf <- shots_25_26_upto %>%
  filter(!PRIMARYTYPE %in% c("corn", "free", "pena")) %>%
  mutate(
    goal_from_shotisgoal = as.integer(SHOTISGOAL == 1),
    goal_from_bodypart   = as.integer(str_detect(tolower(as.character(SHOTBODYPART)), "goal")),
    total_goal           = as.integer(goal_from_shotisgoal == 1 | goal_from_bodypart == 1)
  )

# Common (TEAM/PLAYER/MINUTE) kun for s2
common_shots_min_s2 <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, TEAM_WYID, PLAYER_WYID, MINUTE
  FROM wyscout_matchevents_common
  WHERE PRIMARYTYPE = 'shot'
    AND SEASON_WYID = %s;
", s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

shots_master_s2_upto <- shots_25_26_upto_cpf %>%
  left_join(common_shots_min_s2, by = "EVENT_WYID")

top10_goals_s2 <- shots_master_s2_upto %>%
  left_join(players_role, by = "PLAYER_WYID") %>%
  left_join(team_names, by = "TEAM_WYID") %>%
  group_by(PLAYER_WYID, player_name, TEAMNAME) %>%
  summarise(
    goals = sum(total_goal, na.rm = TRUE),
    xg    = sum(SHOTPOSTSHOTXG, na.rm = TRUE),   # post-shot xG (fordi SHOTXG=0 i dit data)
    shots = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(goals)) %>%
  slice_head(n = 10) %>%
  mutate(
    diff = goals - xg,
    over_under = ifelse(diff > 0, "Overperformer", "Underperformer")
  )

ggplot(top10_goals_s2, aes(x = reorder(player_name, goals))) +
  geom_col(aes(y = goals, fill = TEAMNAME), width = 0.6) +
  geom_point(aes(y = xg), color = "black", size = 3) +
  geom_text(aes(y = goals, label = goals), hjust = -0.2, size = 4) +
  geom_text(aes(y = xg, label = round(xg, 1)), nudge_y = 0.2, size = 3.5, color = "black") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    limits = c(-0.5, max(top10_goals_s2$goals, na.rm = TRUE) * 1.15),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 målscorere i 2025/26 (til og med 08/12/2025)",
    subtitle = "Søjler viser mål - prikker viser post-shot xG",
    x = NULL,
    y = "Antal mål / xG",
    fill = "Hold",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.margin = margin(10, 30, 10, 10))
