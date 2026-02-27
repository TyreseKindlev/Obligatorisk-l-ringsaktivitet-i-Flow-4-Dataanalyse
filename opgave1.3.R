library(RMariaDB)
library(dplyr)
library(ggplot2)
library(stringr)
library(rpart)
library(tidyr)
library(rpart.plot)
## Forbindelse - ###### opgave 1
library(DBI)

con <- dbConnect(
  MariaDB(),
  host     = "www.talmedos.com",
  port     = 3306,
  user     = "dalremote",
  password = "OttoRehagel123456789Long2026!",
  dbname   = "superliga2"
)

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
shots_season_24_25_26 %>%
  summarise(
    n_before = n(),
    n_after  = sum(!PRIMARYTYPE %in% c("corn","free","pena")),
    removed  = sum(PRIMARYTYPE %in% c("corn","free","pena"))
  )


# 5) total_goal (kombiner SHOTISGOAL + "goal" i SHOTBODYPART)
shots_season_24_25_26_cpf <- shots_season_24_25_26_cpf %>%
  mutate(
    goal_from_shotisgoal = as.integer(SHOTISGOAL == 1),
    goal_from_bodypart   = as.integer(str_detect(tolower(as.character(SHOTBODYPART)), "goal")),
    total_goal           = as.integer(goal_from_shotisgoal == 1 | goal_from_bodypart == 1)
  )

# 6) Mål vs ikke mål i procent (side-by-side, rød/blå)
goal_share <- shots_season_24_25_26_cpf %>%
  mutate(outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")) %>%
  count(outcome) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(goal_share, aes(x = outcome, y = pct, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title = "Afslutningseffektivitet: Kun 12% af skud resulterer i mål",
    x = NULL,
    y = "Andel af skud (%)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 7) Hent coords fra common (kun shots)
s1 <- 189918
s2 <- 191611

common_coords <- dbGetQuery(con, sprintf("
SELECT EVENT_WYID, LOCATIONX, LOCATIONY
FROM wyscout_matchevents_common
WHERE SEASON_WYID IN (%s, %s)
  AND PRIMARYTYPE = 'shot';
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# 8) Gennemsnitlig distance til mål (mål vs ikke mål) – meter
mean_dist_goal_m <- shots_season_24_25_26_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    distance_m = sqrt((105 - x_m)^2 + (34 - y_m)^2),
    outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")
  ) %>%
  group_by(outcome) %>%
  summarise(mean_distance_m = mean(distance_m, na.rm = TRUE), .groups = "drop")

ggplot(mean_dist_goal_m, aes(x = outcome, y = mean_distance_m, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_distance_m, 1)), vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title = "Kortere afstand øger sandsynligheden for scoring",
    x = NULL,
    y = "Gennemsnitlig afstand (m)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 9) Gennemsnitlig vinkel til mål (mål vs ikke mål) – meter
mean_angle_goal_m <- shots_season_24_25_26_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
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
    angle_deg_m = ang * 180 / pi,
    
    outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")
  ) %>%
  filter(is.finite(angle_deg_m), angle_deg_m >= 0, angle_deg_m <= 180) %>%
  group_by(outcome) %>%
  summarise(mean_angle_m = mean(angle_deg_m, na.rm = TRUE), .groups = "drop")

ggplot(mean_angle_goal_m, aes(x = outcome, y = mean_angle_m, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_angle_m, 1)), vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title =  "Mål scores fra positioner med større vinkel mod målet",
    x = NULL,
    y = "Gennemsnitlig vinkel (grader)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# D) Gennemsnitlig xG for skud, der ramte målet (mål vs ikke mål) – samme farver
mean_xg_ontarget <- shots_season_24_25_26_cpf %>%
  filter(SHOTONTARGET == 1, !is.na(SHOTXG)) %>%
  mutate(outcome = ifelse(total_goal == 1, "Mål", "Ikke mål")) %>%
  group_by(outcome) %>%
  summarise(
    mean_xg = mean(SHOTXG, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(mean_xg_ontarget, aes(x = outcome, y = mean_xg, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_xg, 3)), 
            vjust = -0.3, 
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
  labs(
    title = "Højere xG øger sandsynligheden for scoring",
    x = NULL,
    y =  "Gennemsnitlig expected goal (XG)",
    fill = NULL,
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
####################################ETC###################################
# 
shots_geo <- shots_season_24_25_26_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    distance_m = sqrt((105 - x_m)^2 + (34 - y_m)^2)
  )

shots_distance_bins <- shots_geo %>%
  mutate(distance_bin = cut(distance_m, breaks = seq(0, 60, by = 5), include.lowest = TRUE)) %>%
  group_by(distance_bin) %>%
  summarise(
    shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    goal_pct = goals / shots * 100,
    .groups = "drop"
  )

ggplot(shots_distance_bins, aes(x = distance_bin, y = goal_pct)) +
  geom_col(fill = "#2C6BA0") +
  geom_text(aes(label = paste0(round(goal_pct, 1), "%")), vjust = -0.4, size = 4, fontface = "bold") +
  labs(
    title = "Jo tættere på mål, jo større målchance",
    x = "Afstand til mål, 5 meters interval",
    y = "Scoringschance (%)",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13)

################################################################################
# 11) Vinkel-bins (20 grader) -> scoringschance (%), vinkel i meter
################################################################################

################################################################################
# 11) Vinkel-bins (20 grader) -> scoringschance (%)
# SAMMENLÆG 120–180 til én gruppe
################################################################################

shots_angle_bins <- shots_geo %>%
  mutate(
    goal_x_m = 105,
    left_post_y_m  = 34 - 7.32/2,
    right_post_y_m = 34 + 7.32/2,
    a1 = atan2(left_post_y_m  - y_m, goal_x_m - x_m),
    a2 = atan2(right_post_y_m - y_m, goal_x_m - x_m),
    ang = abs(a2 - a1),
    ang = ifelse(ang > pi, 2*pi - ang, ang),
    angle_deg_m = ang * 180 / pi,
    
    angle_bin = case_when(
      angle_deg_m <= 20   ~ "[0,20]",
      angle_deg_m <= 40   ~ "(20,40]",
      angle_deg_m <= 60   ~ "(40,60]",
      angle_deg_m <= 80   ~ "(60,80]",
      angle_deg_m <= 100  ~ "(80,100]",
      angle_deg_m <= 120  ~ "(100,120]",
      angle_deg_m > 120   ~ "(120,180]"
    ),
    angle_bin = factor(
      angle_bin,
      levels = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]", "(100,120]", "(120,180]")
    )
  ) %>%
  filter(is.finite(angle_deg_m), angle_deg_m >= 0, angle_deg_m <= 180) %>%
  group_by(angle_bin) %>%
  summarise(
    shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    goal_pct = goals / shots * 100,
    .groups = "drop"
  ) %>%
  arrange(angle_bin)

ggplot(shots_angle_bins, aes(x = angle_bin, y = goal_pct)) +
  geom_col(fill = "#2C6BA0") +
  geom_text(
    aes(label = paste0(round(goal_pct, 1), "%")),
    vjust = -0.4,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Jo større vinkel til mål, desto højere scoringschance",
    subtitle = "Vinkel beregnes som åbningen mellem linjerne til venstre/højre stolpe",
    x = "Vinkel til mål (grader)",
    y = "Scoringschance (%)",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13)
######################################

# xG-bins -> faktisk scoringschance (%)
# (bruger total_goal og SHOTXG fra jeres 6885 skud)

xg_bins <- shots_season_24_25_26_cpf %>%
  filter(!is.na(SHOTXG)) %>%
  mutate(
    xg = as.numeric(SHOTXG),
    xg_bin = cut(xg, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)
  ) %>%
  group_by(xg_bin) %>%
  summarise(
    shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    goal_pct = goals / shots * 100,
    mean_xg = mean(xg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 1)  # fjerner bins med meget få skud (mere stabilt)

ggplot(xg_bins, aes(x = xg_bin, y = goal_pct)) +
  geom_col(fill = "#2C6BA0") +
  geom_text(aes(label = paste0(round(goal_pct, 1), "%")), vjust = -0.4, size = 4, fontface = "bold") +
  labs(
    title = "Jo højere xG, desto højere scoringschance",
    subtitle = "Faktisk målrate pr xG-interval (bins med mindst 30 skud)",
    x = "SHOTXG (0.05 intervaller)",
    y = "Scoringschance (%)",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13)

########TID########################
common_time <- dbGetQuery(con, sprintf("
SELECT EVENT_WYID, MINUTE
FROM wyscout_matchevents_common
WHERE SEASON_WYID IN (%s, %s)
  AND PRIMARYTYPE = 'shot';
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

shots_time <- shots_season_24_25_26_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_time, by = "EVENT_WYID") %>%
  filter(!is.na(MINUTE)) %>%
  mutate(
    MINUTE = as.numeric(MINUTE),
    time_bin = cut(MINUTE, breaks = c(0,15,30,45,60,75,90,120),
                   include.lowest = TRUE, right = TRUE)
  ) %>%
  group_by(time_bin) %>%
  summarise(
    shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    goal_pct = goals / shots * 100,
    .groups = "drop"
  )

ggplot(shots_time, aes(x = time_bin, y = goal_pct)) +
  geom_col(fill = "#2C6BA0") +
  geom_text(aes(label = paste0(round(goal_pct, 1), "%")), vjust = -0.4, size = 4, fontface = "bold") +
  labs(
    title = "Scoringschance i forhold til tid i kampen",
    subtitle = "Målrate pr 15-min interval",
    x = "Kampminut (15-min intervaller)",
    y = "Scoringschance (%)",
    caption = "Kilde: Wyscout"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13)

library(dplyr)
library(ggplot2)

precision_full_df <- shots_season_24_25_26_cpf %>%
  summarise(
    total_shots = n(),
    on_target = sum(SHOTONTARGET == 1, na.rm = TRUE),
    off_target = sum(SHOTONTARGET == 0, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = c(on_target, off_target),
    names_to = "outcome",
    values_to = "count"
  ) %>%
  mutate(
    pct = count / sum(count) * 100,
    outcome = ifelse(outcome == "on_target",
                     "Rammer målet",
                     "Rammer ikke målet")
  )
  theme_minimal()

  ggplot(precision_full_df, aes(x = outcome, y = pct, fill = outcome)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(pct,1), "%")),
              vjust = -0.4,
              size = 6) +
    scale_fill_manual(values = c("Rammer målet" = "steelblue",
                                 "Rammer ikke målet" = "red")) +
    labs(
      title = "Skudpræcision: Andel der rammer målet",
      x = NULL,
      y = "Procent af skud"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  # mean_xg_ontarget forventes at have: outcome, mean_xg, n
  # outcome = "Mål" / "Ikke mål"
  
  mean_xg_ontarget_plot <- mean_xg_ontarget %>%
    mutate(
      outcome = factor(outcome, levels = c("Ikke mål", "Mål")),
      label = paste0("xG = ", round(mean_xg, 3), "\n(n = ", n, ")")
    )
  
  ggplot(mean_xg_ontarget_plot, aes(x = outcome, y = mean_xg, fill = outcome)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = label), vjust = -0.4, size = 5, fontface = "bold") +
    scale_fill_manual(values = c("Ikke mål" = "red", "Mål" = "steelblue")) +
    scale_y_continuous(
      limits = c(0, max(mean_xg_ontarget_plot$mean_xg) * 1.15),
      labels = number_format(accuracy = 0.001)
    ) +
    labs(
      title = "Chancekvalitet for skud på mål: mål vs. reddet",
      subtitle = "Mål har højere gennemsnitlig xG blandt skud der rammer målet",
      x = NULL,
      y = "Gennemsnitlig xG (SHOTXG)",
      caption = "Kilde: Wyscout"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  