library(RMariaDB)
library(dplyr)
library(ggplot2)
library(stringr)
library(rpart)
library(tidyr)
library(rpart.plot)
library(randomForest)
## Forbindelse
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
    
# 6) Hovedskud + hovedskudsmål pr. sæson (via secondarytypes: head_shot + goal/penalty_goal)

# hent secondarytypes (kun de kolonner vi skal bruge)
secondary_types <- dbGetQuery(con, "
SELECT COMPETITION_WYID, MATCH_WYID, EVENT_WYID,
       SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
       SECONDARYTYPE6, SECONDARYTYPE7, SECONDARYTYPE8, SECONDARYTYPE9, SECONDARYTYPE10
FROM wyscout_matchevents_secondarytype
WHERE COMPETITION_WYID = 335;
")

st_flags <- secondary_types %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  transmute(
    EVENT_WYID,
    head_shot_st = as.integer(
      SECONDARYTYPE1 == "head_shot" |
        SECONDARYTYPE2 == "head_shot" |
        SECONDARYTYPE3 == "head_shot" |
        SECONDARYTYPE4 == "head_shot" |
        SECONDARYTYPE5 == "head_shot" |
        SECONDARYTYPE6 == "head_shot" |
        SECONDARYTYPE7 == "head_shot" |
        SECONDARYTYPE8 == "head_shot" |
        SECONDARYTYPE9 == "head_shot" |
        SECONDARYTYPE10 == "head_shot"
    ),
    goal_st = as.integer(
      SECONDARYTYPE1 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE2 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE3 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE4 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE5 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE6 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE7 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE8 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE9 %in% c("goal", "penalty_goal") |
        SECONDARYTYPE10 %in% c("goal", "penalty_goal")
    )
  ) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

header_table_by_season <- shots_season_24_25_26_cpf %>%
  filter(SEASON_WYID %in% c(189918, 191611)) %>%
  select(SEASON_WYID, EVENT_WYID) %>%
  distinct() %>%
  left_join(st_flags, by = "EVENT_WYID") %>%
  mutate(
    head_shot_st = ifelse(is.na(head_shot_st), 0L, head_shot_st),
    goal_st      = ifelse(is.na(goal_st), 0L, goal_st)
  ) %>%
  group_by(SEASON_WYID) %>%
  summarise(
    head_shots = sum(head_shot_st == 1),
    head_goals = sum(head_shot_st == 1 & goal_st == 1),
    head_goal_rate = head_goals / head_shots,
    .groups = "drop"
  ) %>%
  arrange(SEASON_WYID)

header_table_by_season

# 7) Hent de 352 assist-events for sidste sæson (Superliga 24/25 = 189918)
matches_2425 <- dbGetQuery(con, "
SELECT MATCH_WYID
FROM wyscout_matches
WHERE COMPETITION_WYID = 335
  AND SEASON_WYID = 189918;
")$MATCH_WYID

assists_2425 <- secondary_types %>%
  filter(MATCH_WYID %in% matches_2425) %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  mutate(
    has_assist = as.integer(
      SECONDARYTYPE1  == "assist" | SECONDARYTYPE2  == "assist" |
        SECONDARYTYPE3  == "assist" | SECONDARYTYPE4  == "assist" |
        SECONDARYTYPE5  == "assist" | SECONDARYTYPE6  == "assist" |
        SECONDARYTYPE7  == "assist" | SECONDARYTYPE8  == "assist" |
        SECONDARYTYPE9  == "assist" | SECONDARYTYPE10 == "assist"
    )
  ) %>%
  filter(has_assist == 1) %>%
  select(MATCH_WYID, EVENT_WYID) %>%
  distinct()

# kontrol: skal være 352
assists_2425 %>% summarise(assist_events = n_distinct(EVENT_WYID))
###############################
# Assists for begge sæsoner + samlet (kræver: secondary_types er hentet som i din kode)
matches_both <- dbGetQuery(con, "
  SELECT MATCH_WYID, SEASON_WYID
  FROM wyscout_matches
  WHERE COMPETITION_WYID = 335
    AND SEASON_WYID IN (189918, 191611);
")

assists_both <- secondary_types %>%
  semi_join(matches_both, by = "MATCH_WYID") %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  mutate(
    has_assist = as.integer(
      SECONDARYTYPE1 == "assist" | SECONDARYTYPE2 == "assist" | SECONDARYTYPE3 == "assist" |
        SECONDARYTYPE4 == "assist" | SECONDARYTYPE5 == "assist" | SECONDARYTYPE6 == "assist" |
        SECONDARYTYPE7 == "assist" | SECONDARYTYPE8 == "assist" | SECONDARYTYPE9 == "assist" |
        SECONDARYTYPE10 == "assist"
    )
  ) %>%
  filter(has_assist == 1) %>%
  select(MATCH_WYID, EVENT_WYID) %>%
  distinct() %>%
  left_join(matches_both, by = "MATCH_WYID")

# (a) Assist-events pr. sæson
assist_counts_by_season <- assists_both %>%
  group_by(SEASON_WYID) %>%
  summarise(assist_events = n_distinct(EVENT_WYID), .groups = "drop") %>%
  arrange(SEASON_WYID)

# (b) Samlet for begge sæsoner
assist_counts_total <- assists_both %>%
  summarise(assist_events_total = n_distinct(EVENT_WYID))

assist_counts_by_season
assist_counts_total
##################################
##################################
##################################
##################################
# 8) DISTANCE + VINKEL + "Mål/Ikke mål" (i forlængelse af din kode)
##################################

s1 <- 189918
s2 <- 191611

# Hent coords fra common (kun shots)
common_coords <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, LOCATIONX, LOCATIONY
  FROM wyscout_matchevents_common
  WHERE COMPETITION_WYID = 335
    AND SEASON_WYID IN (%s, %s)
    AND PRIMARYTYPE = 'shot';
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# Join + beregn distance og vinkel + label outcome
shots_xg <- shots_season_24_25_26_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_coords, by = "EVENT_WYID") %>%
  filter(!is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    # Label
    outcome = ifelse(total_goal == 1, "Mål", "Ikke mål"),
    
    # Wyscout 0-100 -> bane i meter (105x68)
    x_m = as.numeric(LOCATIONX) / 100 * 105,
    y_m = as.numeric(LOCATIONY) / 100 * 68,
    
    # afstand til målmidte (105, 34)
    distance_m = sqrt((105 - x_m)^2 + (34 - y_m)^2),
    
    # vinkel til mål (mellem linjer til stolper)
    goal_x_m = 105,
    left_post_y_m  = 34 - 7.32/2,
    right_post_y_m = 34 + 7.32/2,
    
    a1 = atan2(left_post_y_m  - y_m, goal_x_m - x_m),
    a2 = atan2(right_post_y_m - y_m, goal_x_m - x_m),
    ang = abs(a2 - a1),
    ang = ifelse(ang > pi, 2*pi - ang, ang),
    angle_deg_m = ang * 180 / pi
  ) %>%
  filter(is.finite(distance_m), is.finite(angle_deg_m), angle_deg_m >= 0, angle_deg_m <= 180)

# Tjek fordeling mål/ikke mål + basis stats
shots_xg %>%
  summarise(
    n_shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    no_goals = sum(total_goal == 0, na.rm = TRUE),
    goal_pct = mean(total_goal == 1, na.rm = TRUE) * 100
  )

# (valgfrit) hurtig tabel du kan printe i opgaven
shots_xg %>%
  count(outcome) %>%
  mutate(pct = n / sum(n) * 100)
##################################
# 9) xA (expected assists) – vi bygger videre på jeres proces
#
# Hvorfor gør vi det sådan?
# - Vi vil måle "hvor gode chancer skaber afleveringer?" uden at snyde (data leakage).
# - Hvis vi bare bruger et "assist"-flag der kun findes når der blev mål, så får modellen facit.
# - Derfor laver vi xA som: xA = xG på det efterfølgende skud, som afleveringen skabte.
# - Vi kobler assist-event -> næste shot i samme POSSESSION (samme angreb), så vi ikke gætter.
##################################

# Sæsoner (samme som før)
s1 <- 189918
s2 <- 191611

# 1) Head_shot-flag på shot-events (bruges i xG-modellen)
headshot_flags <- secondary_types %>%
  semi_join(matches_both, by = "MATCH_WYID") %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  transmute(
    EVENT_WYID = as.character(EVENT_WYID),
    head_shot = as.integer(
      SECONDARYTYPE1 == "head_shot" | SECONDARYTYPE2 == "head_shot" | SECONDARYTYPE3 == "head_shot" |
        SECONDARYTYPE4 == "head_shot" | SECONDARYTYPE5 == "head_shot" | SECONDARYTYPE6 == "head_shot" |
        SECONDARYTYPE7 == "head_shot" | SECONDARYTYPE8 == "head_shot" | SECONDARYTYPE9 == "head_shot" |
        SECONDARYTYPE10 == "head_shot"
    )
  ) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

shots_xg_xa <- shots_xg %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(headshot_flags, by = "EVENT_WYID") %>%
  mutate(head_shot = ifelse(is.na(head_shot), 0L, head_shot))

# 2) Fit xG-model (klassisk baseline: distance + vinkel + head_shot)
#    (xG er sandsynligheden for mål for hvert shot)
xg_model_xa <- glm(
  total_goal ~ distance_m + angle_deg_m + head_shot,
  data = shots_xg_xa,
  family = binomial()
)

shots_xg_xa <- shots_xg_xa %>%
  mutate(xg = predict(xg_model_xa, newdata = shots_xg_xa, type = "response"))

# 3) Find assist-events via secondary_types (begge sæsoner)
assist_events <- secondary_types %>%
  semi_join(matches_both, by = "MATCH_WYID") %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  mutate(
    is_assist = as.integer(
      SECONDARYTYPE1 == "assist" | SECONDARYTYPE2 == "assist" | SECONDARYTYPE3 == "assist" |
        SECONDARYTYPE4 == "assist" | SECONDARYTYPE5 == "assist" | SECONDARYTYPE6 == "assist" |
        SECONDARYTYPE7 == "assist" | SECONDARYTYPE8 == "assist" | SECONDARYTYPE9 == "assist" |
        SECONDARYTYPE10 == "assist"
    )
  ) %>%
  filter(is_assist == 1) %>%
  transmute(assist_event = as.character(EVENT_WYID), MATCH_WYID) %>%
  distinct()

# 4) Hent common events for at få POSSESSION_WYID og rækkefølge (POSSESSIONEVENTINDEX)
common_both <- dbGetQuery(con, sprintf("
  SELECT
    EVENT_WYID, MATCH_WYID, SEASON_WYID, PRIMARYTYPE,
    POSSESSION_WYID, POSSESSIONEVENTINDEX,
    PLAYER_WYID, TEAM_WYID
  FROM wyscout_matchevents_common
  WHERE COMPETITION_WYID = 335
    AND SEASON_WYID IN (%s, %s)
    AND POSSESSION_WYID IS NOT NULL
    AND POSSESSIONEVENTINDEX IS NOT NULL;
", s1, s2)) %>%
  mutate(
    EVENT_WYID = as.character(EVENT_WYID),
    POSSESSION_WYID = as.character(POSSESSION_WYID),
    POSSESSIONEVENTINDEX = as.numeric(POSSESSIONEVENTINDEX)
  )

# Assist-eventets position i angrebet (possession + index)
assist_common <- common_both %>%
  inner_join(assist_events, by = c("EVENT_WYID" = "assist_event", "MATCH_WYID" = "MATCH_WYID")) %>%
  transmute(
    assist_event = EVENT_WYID,
    MATCH_WYID, SEASON_WYID,
    POSSESSION_WYID,
    assist_index = POSSESSIONEVENTINDEX,
    assist_player = PLAYER_WYID,
    assist_team = TEAM_WYID
  )

# Shot-events i samme possession (til at finde "næste shot")
shot_common <- common_both %>%
  filter(PRIMARYTYPE == "shot") %>%
  transmute(
    shot_event = EVENT_WYID,
    MATCH_WYID, SEASON_WYID,
    POSSESSION_WYID,
    shot_index = POSSESSIONEVENTINDEX
  )

# 5) Link: for hver assist -> første shot efter assist i samme possession
assist_to_shot <- assist_common %>%
  inner_join(shot_common, by = c("MATCH_WYID","SEASON_WYID","POSSESSION_WYID")) %>%
  filter(shot_index > assist_index) %>%
  group_by(assist_event) %>%
  slice_min(order_by = shot_index, n = 1, with_ties = FALSE) %>%
  ungroup()

# 6) xA = xG på det linked shot (og evt. om skuddet blev mål)
assist_xa <- assist_to_shot %>%
  left_join(
    shots_xg_xa %>% transmute(shot_event = EVENT_WYID, xg, total_goal),
    by = "shot_event"
  ) %>%
  mutate(
    xa = xg,
    goal_from_assist = as.integer(total_goal == 1)
  )

# 7) Output (samme format som du allerede viste)
xa_summary_by_season <- assist_xa %>%
  group_by(SEASON_WYID) %>%
  summarise(
    assist_events = n_distinct(assist_event),
    linked_shots  = n_distinct(shot_event),
    goals         = sum(goal_from_assist, na.rm = TRUE),
    total_xa      = sum(xa, na.rm = TRUE),
    mean_xa       = mean(xa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SEASON_WYID)

xa_summary_overall <- assist_xa %>%
  summarise(
    assist_events = n_distinct(assist_event),
    linked_shots  = n_distinct(shot_event),
    goals         = sum(goal_from_assist, na.rm = TRUE),
    total_xa      = sum(xa, na.rm = TRUE),
    mean_xa       = mean(xa, na.rm = TRUE)
  )

xa_summary_by_season
xa_summary_overall
####
##################################
# 9) INDEN I BOKSEN (feature) + graf
# (kør efter du har lavet shots_xg, og FØR du fitter xG)
#
# Hvorfor?
# - "Inside box" er en simpel, intuitiv feature der opsummerer position:
#   skud tættere på mål og mere centralt ligger ofte inde i boksen.
# - Den kan bruges både til beskrivelse (målrate inde/ude)
#   og som ekstra variabel i xG-modellen bagefter.
##################################

shots_xg <- shots_xg %>%
  mutate(
    # Straffesparksfelt: 16.5m fra mållinje og 40.32m bredt (±20.16m fra midten y=34)
    inside_box = as.integer(
      x_m >= (105 - 16.5) &
        y_m >= (34 - 20.16) &
        y_m <= (34 + 20.16)
    ),
    box_label = ifelse(inside_box == 1, "Inden i boksen", "Udenfor boksen")
  )

box_summary <- shots_xg %>%
  group_by(box_label) %>%
  summarise(
    shots = n(),
    goals = sum(total_goal, na.rm = TRUE),
    goal_pct = goals / shots * 100,
    .groups = "drop"
  )

print(box_summary)

ggplot(box_summary, aes(x = box_label, y = goal_pct, fill = box_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(goal_pct, 1), "%\n(n=", shots, ")")), vjust = -0.3, size = 5) +
  labs(
    title = "Målrate: inden i boksen vs udenfor",
    subtitle = "Straffesparksfelt defineret som 16.5m fra mållinjen og 40.32m bredt",
    x = NULL,
    y = "Målrate (%)",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

###################################################################
############XG kan starte nu#####################################
##################################################################
##################################
# xG-model + K-fold validation 
# - Features: distance_m, angle_deg_m, head_shot, inside_box
# - Metrics pr fold: LogLoss, Brier, Accuracy (0.5 threshold)
#
# KRAV: Kør efter du har lavet:
#   shots_xg, secondary_types, matches_both
# (og at shots_xg har x_m, y_m, distance_m, angle_deg_m, total_goal)
##################################

set.seed(44)
K <- 10

# 1) Head_shot flag (på shot-events)
headshot_flags <- secondary_types %>%
  semi_join(matches_both, by = "MATCH_WYID") %>%
  mutate(across(starts_with("SECONDARYTYPE"), as.character)) %>%
  transmute(
    EVENT_WYID = as.character(EVENT_WYID),
    head_shot = as.integer(
      SECONDARYTYPE1 == "head_shot" | SECONDARYTYPE2 == "head_shot" | SECONDARYTYPE3 == "head_shot" |
        SECONDARYTYPE4 == "head_shot" | SECONDARYTYPE5 == "head_shot" | SECONDARYTYPE6 == "head_shot" |
        SECONDARYTYPE7 == "head_shot" | SECONDARYTYPE8 == "head_shot" | SECONDARYTYPE9 == "head_shot" |
        SECONDARYTYPE10 == "head_shot"
    )
  ) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# 2) Modeldata = shots_xg + inside_box + head_shot
xg_data <- shots_xg %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(headshot_flags, by = "EVENT_WYID") %>%
  mutate(
    head_shot = ifelse(is.na(head_shot), 0L, head_shot),
    
    # inside_box (straffesparksfelt)
    inside_box = as.integer(
      x_m >= (105 - 16.5) &
        y_m >= (34 - 20.16) &
        y_m <= (34 + 20.16)
    )
  ) %>%
  filter(
    !is.na(total_goal),
    is.finite(distance_m), is.finite(angle_deg_m),
    !is.na(inside_box), !is.na(head_shot)
  )

# 3) Fold-indeks
n <- nrow(xg_data)
fold_id <- sample(rep(1:K, length.out = n))

# 4) Hjælpefunktioner til metrics
clip01 <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

logloss <- function(y, p){
  p <- clip01(p)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(y, p){
  mean((y - p)^2)
}

acc05 <- function(y, p){
  mean((p >= 0.5) == (y == 1))
}

# 5) K-fold CV loop
cv_res <- vector("list", K)

for(k in 1:K){
  train <- xg_data[fold_id != k, ]
  test  <- xg_data[fold_id == k, ]
  
  m <- glm(
    total_goal ~ distance_m + angle_deg_m + head_shot + inside_box,
    data = train,
    family = binomial()
  )
  
  p <- predict(m, newdata = test, type = "response")
  
  cv_res[[k]] <- data.frame(
    fold = k,
    n_test = nrow(test),
    goals_test = sum(test$total_goal),
    logloss = logloss(test$total_goal, p),
    brier = brier(test$total_goal, p),
    acc_05 = acc05(test$total_goal, p)
  )
}

cv_res <- dplyr::bind_rows(cv_res)

# 6) Samlet CV-resultat (vægtet efter fold-størrelse)
cv_summary <- cv_res %>%
  summarise(
    K = K,
    n_total = sum(n_test),
    goals_total = sum(goals_test),
    logloss_mean = weighted.mean(logloss, n_test),
    brier_mean   = weighted.mean(brier, n_test),
    acc05_mean   = weighted.mean(acc_05, n_test),
    logloss_sd = sd(logloss),
    brier_sd   = sd(brier),
    acc05_sd   = sd(acc_05)
  )

print(cv_res)
print(cv_summary)

# 7) Fit "final" model på alle data + gem xG på hver observation
xg_model_final <- glm(
  total_goal ~ distance_m + angle_deg_m + head_shot + inside_box,
  data = xg_data,
  family = binomial()
)

xg_data <- xg_data %>%
  mutate(xg_final = predict(xg_model_final, newdata = xg_data, type = "response"))

summary(xg_model_final)

####################################################
# RANDOM FOREST – 10 fold CV
######################################################


library(randomForest)

set.seed(10)
K <- 5

# Brug samme modeldata som før
rf_data <- xg_data %>%
  select(total_goal, distance_m, angle_deg_m, head_shot, inside_box)

n <- nrow(rf_data)
fold_id <- sample(rep(1:K, length.out = n))

clip01 <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

logloss <- function(y, p){
  p <- clip01(p)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(y, p){
  mean((y - p)^2)
}

acc05 <- function(y, p){
  mean((p >= 0.5) == (y == 1))
}

cv_rf <- vector("list", K)

for(k in 1:K){
  
  train <- rf_data[fold_id != k, ]
  test  <- rf_data[fold_id == k, ]
  
  m <- randomForest(
    as.factor(total_goal) ~ distance_m + angle_deg_m + head_shot + inside_box,
    data = train,
    ntree = 500,
    mtry = 2
  )
  
  p <- predict(m, newdata = test, type = "prob")[,2]
  
  cv_rf[[k]] <- data.frame(
    fold = k,
    logloss = logloss(test$total_goal, p),
    brier = brier(test$total_goal, p),
    acc05 = acc05(test$total_goal, p)
  )
}

cv_rf <- bind_rows(cv_rf)

cv_rf_summary <- cv_rf %>%
  summarise(
    logloss_mean = mean(logloss),
    brier_mean = mean(brier),
    acc05_mean = mean(acc05),
    logloss_sd = sd(logloss),
    brier_sd = sd(brier),
    acc05_sd = sd(acc05)
  )

print(cv_rf_summary)
##################################
# VISUELT BESLUTNINGSTRÆ (rpart) på jeres xG-features
# bruger: distance_m, angle_deg_m, head_shot, inside_box
# data: xg_data (som du allerede har fra CV-koden)
##################################

set.seed(12)

tree_data <- xg_data %>%
  select(total_goal, distance_m, angle_deg_m, head_shot, inside_box) %>%
  mutate(total_goal = factor(total_goal, levels = c(0,1), labels = c("Ikke mål","Mål")))

xg_tree <- rpart(
  total_goal ~ distance_m + angle_deg_m + head_shot + inside_box,
  data = tree_data,
  method = "class",
  control = rpart.control(cp = 0.002, minsplit = 50, maxdepth = 4)
)

rpart.plot(
  xg_tree,
  type = 2,
  extra = 104,          # viser klasse + sandsynlighed + n
  fallen.leaves = TRUE,
  box.palette = "RdBu",
  under = TRUE,
  faclen = 0
)

