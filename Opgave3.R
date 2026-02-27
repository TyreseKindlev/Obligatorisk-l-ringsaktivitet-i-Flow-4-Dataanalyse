###########################
# OPGAVE 3.1 – Clustering på afleveringer (hver aflevering = én række)
# Standalone: connecter -> henter data -> bygger features -> filtrerer (0,0) -> kmeans -> plot på bane
###########################

library(RMariaDB)
library(DBI)
library(dplyr)
library(ggplot2)

# -----------------------
# Forbindelse
# -----------------------
con <- dbConnect(
  MariaDB(),
  host     = "www.talmedos.com",
  port     = 3306,
  user     = "dalremote",
  password = "OttoRehagel123456789Long2026!",
  dbname   = "superliga2"
)

# -----------------------
# Sæsoner
# -----------------------
s1 <- 189918
s2 <- 191611
seasons_tokeep <- c(s1, s2)

# -----------------------
# Matches (kun de valgte sæsoner)
# -----------------------
matches <- dbGetQuery(con, sprintf("
  SELECT MATCH_WYID, SEASON_WYID
  FROM wyscout_matches
  WHERE competition_wyid IN (335, 328)
    AND SEASON_WYID IN (%s, %s);
", s1, s2)) %>%
  distinct(MATCH_WYID, .keep_all = TRUE)

# -----------------------
# Passes (TEAM/PLAYER ligger ikke her)
# -----------------------
passes <- dbGetQuery(con, "
  SELECT
    EVENT_WYID, MATCH_WYID, PRIMARYTYPE,
    LENGTH, ANGLE,
    ENDLOCATIONX, ENDLOCATIONY,
    RECIPIENT_WYID
  FROM wyscout_matchevents_passes
  WHERE COMPETITION_WYID IN (335, 328);
") %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# -----------------------
# Common: kun pass-events (giver korrekte start-koordinater + TEAM/PLAYER)
# -----------------------
common_pass_xy <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, TEAM_WYID, PLAYER_WYID, LOCATIONX, LOCATIONY
  FROM wyscout_matchevents_common
  WHERE SEASON_WYID IN (%s, %s)
    AND PRIMARYTYPE = 'pass';
", s1, s2)) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

# -----------------------
# Join: matches -> passes -> common (start coords)
# -----------------------
passes_fix <- matches %>%
  left_join(passes, by = "MATCH_WYID") %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_pass_xy, by = "EVENT_WYID")

# --- 0) Safety: hvis RECIPIENT_WYID ikke findes i passes-tabellen, så hent uden den
passes <- tryCatch(
  dbGetQuery(con, "
    SELECT
      EVENT_WYID, MATCH_WYID, PRIMARYTYPE,
      LENGTH, ANGLE,
      ENDLOCATIONX, ENDLOCATIONY,
      RECIPIENT_WYID
    FROM wyscout_matchevents_passes
    WHERE COMPETITION_WYID IN (335, 328);
  "),
  error = function(e) {
    message("RECIPIENT_WYID findes ikke -> henter passes uden RECIPIENT_WYID")
    dbGetQuery(con, "
      SELECT
        EVENT_WYID, MATCH_WYID, PRIMARYTYPE,
        LENGTH, ANGLE,
        ENDLOCATIONX, ENDLOCATIONY
      FROM wyscout_matchevents_passes
      WHERE COMPETITION_WYID IN (335, 328);
    ")
  }
) %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)


# --- 1) Rebuild passes_fix (så vi er sikre på at passes er den rigtige)
passes_fix <- matches %>%
  left_join(passes, by = "MATCH_WYID") %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_pass_xy, by = "EVENT_WYID")

# --- 2) Kun rækker med start coords (til zoner)
passes_start <- passes_fix %>%
  filter(!is.na(TEAM_WYID), !is.na(LOCATIONX), !is.na(LOCATIONY)) %>%
  mutate(
    x = as.numeric(LOCATIONX),
    y = as.numeric(LOCATIONY)
  ) %>%
  filter(x >= 0, x <= 100, y >= 0, y <= 100)

cat("Rows in passes_start:", nrow(passes_start), "\n")
cat("Teams:", n_distinct(passes_start$TEAM_WYID), "\n")

# --- 3) Zoner (6x4) + hold-profiler (andel pr zone)
nx <- 6
ny <- 4
zones <- paste0("z", rep(1:nx, each = ny), "_", rep(1:ny, times = nx))

team_zone <- passes_start %>%
  mutate(
    zx = pmin(floor(x / (100 / nx)) + 1, nx),
    zy = pmin(floor(y / (100 / ny)) + 1, ny),
    zone = paste0("z", zx, "_", zy)
  ) %>%
  count(TEAM_WYID, zone) %>%
  group_by(TEAM_WYID) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  select(TEAM_WYID, zone, share) %>%
  complete(TEAM_WYID, zone = zones, fill = list(share = 0)) %>%
  pivot_wider(names_from = zone, values_from = share) %>%
  arrange(TEAM_WYID)

X <- as.matrix(team_zone[, -1])
rownames(X) <- as.character(team_zone$TEAM_WYID)

# --- 4) Kmeans (k=4) på hold-profiler
set.seed(1)
km4 <- kmeans(X, centers = 4, nstart = 50)

cluster_df4 <- data.frame(
  TEAM_WYID = as.integer(rownames(X)),
  cluster  = km4$cluster
) %>% arrange(cluster, TEAM_WYID)

cat("\nCluster sizes (k=4):\n")
print(table(cluster_df4$cluster))

# --- 5) Holdnavne + tabel med alle hold fordelt på cluster
teams <- dbGetQuery(con, "
  SELECT TEAM_WYID, TEAMNAME, OFFICIALNAME
  FROM wyscout_teams;
") %>%
  mutate(TEAM_WYID = as.integer(TEAM_WYID)) %>%
  distinct(TEAM_WYID, .keep_all = TRUE) %>%
  mutate(TEAM_DISPLAY = ifelse(!is.na(OFFICIALNAME) & OFFICIALNAME != "", OFFICIALNAME, TEAMNAME)) %>%
  select(TEAM_WYID, TEAM_DISPLAY)

cluster_named4 <- cluster_df4 %>%
  left_join(teams, by = "TEAM_WYID") %>%
  arrange(cluster, TEAM_DISPLAY)

cat("\nAlle hold fordelt på 4 clusters (k=4):\n")
print(cluster_named4)

# (Valgfrit) Hvis du vil lave varme/kolde plots igen, så sig til – så bygger vi dem på cluster_named4.
###########################
# OPGAVE 3.2 – 1. division scouting + spiller-clusters i FCN hot zones (k=3) + pænt PCA plot (uden navne)
# Kører direkte videre på din eksisterende kode og connection `con`
# Krav: din kode ovenfor skal have kørt, så vi har `passes_start` (fra Superliga+1.div matches)
###########################

library(dplyr)
library(ggplot2)
library(tidyr)

# -----------------------
# A) FCN hot zones (fra SUPERLIGA-data du allerede har i passes_start)
# -----------------------
fcn_id <- 7458
nx_hot <- 6
ny_hot <- 4
top_n_hot <- 6

passes_start_z <- passes_start %>%
  mutate(
    zx = pmin(floor(x / (100 / nx_hot)) + 1, nx_hot),
    zy = pmin(floor(y / (100 / ny_hot)) + 1, ny_hot),
    zone = paste0("z", zx, "_", zy)
  )

hot_zones <- passes_start_z %>%
  filter(TEAM_WYID == fcn_id) %>%
  count(zone, sort = TRUE) %>%
  mutate(share = n / sum(n)) %>%
  slice_head(n = top_n_hot)

cat("\nFCN hot zones (top", top_n_hot, "):\n")
print(hot_zones)

# -----------------------
# B) 1. division (COMP=328) – nuværende sæson findes automatisk (seneste last_date)
# -----------------------
div1_season_df <- dbGetQuery(con, "
  SELECT SEASON_WYID,
         COUNT(*) AS n_matches,
         MIN(date) AS first_date,
         MAX(date) AS last_date
  FROM wyscout_matches
  WHERE COMPETITION_WYID = 328
  GROUP BY SEASON_WYID
  ORDER BY last_date DESC, n_matches DESC;
")
div1_season <- as.integer(div1_season_df$SEASON_WYID[1])

cat("\nValgt 1. division sæson (seneste):", div1_season, "\n")
print(head(div1_season_df, 5))

# -----------------------
# C) Byg 1. division passes med "accurate" via recipient-team (matchdetail_players)
# -----------------------
matches_div1 <- dbGetQuery(con, sprintf("
  SELECT MATCH_WYID
  FROM wyscout_matches
  WHERE COMPETITION_WYID = 328
    AND SEASON_WYID = %s;
", div1_season)) %>%
  distinct(MATCH_WYID, .keep_all = TRUE) %>%
  mutate(MATCH_WYID = as.integer(MATCH_WYID))

passes_div1_rec <- dbGetQuery(con, "
  SELECT EVENT_WYID, MATCH_WYID, RECIPIENT_WYID
  FROM wyscout_matchevents_passes
  WHERE COMPETITION_WYID = 328;
") %>%
  mutate(
    EVENT_WYID = as.character(EVENT_WYID),
    MATCH_WYID = as.integer(MATCH_WYID),
    RECIPIENT_WYID = as.integer(RECIPIENT_WYID)
  ) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

common_div1 <- dbGetQuery(con, sprintf("
  SELECT EVENT_WYID, TEAM_WYID, PLAYER_WYID, LOCATIONX, LOCATIONY
  FROM wyscout_matchevents_common
  WHERE COMPETITION_WYID = 328
    AND SEASON_WYID = %s
    AND PRIMARYTYPE = 'pass';
", div1_season)) %>%
  mutate(
    EVENT_WYID = as.character(EVENT_WYID),
    TEAM_WYID = as.integer(TEAM_WYID),
    PLAYER_WYID = as.integer(PLAYER_WYID)
  ) %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

mdp_div1 <- dbGetQuery(con, sprintf("
  SELECT MATCH_WYID, TEAM_WYID, PLAYER_WYID
  FROM wyscout_matchdetail_players
  WHERE COMPETITION_WYID = 328
    AND MATCH_WYID IN (%s);
", paste(matches_div1$MATCH_WYID, collapse = ","))) %>%
  mutate(
    MATCH_WYID = as.integer(MATCH_WYID),
    TEAM_WYID = as.integer(TEAM_WYID),
    PLAYER_WYID = as.integer(PLAYER_WYID)
  ) %>%
  distinct(MATCH_WYID, PLAYER_WYID, .keep_all = TRUE)

passes_full_div1 <- matches_div1 %>%
  left_join(passes_div1_rec, by = "MATCH_WYID") %>%
  left_join(common_div1, by = "EVENT_WYID") %>%
  filter(!is.na(TEAM_WYID), !is.na(PLAYER_WYID),
         !is.na(LOCATIONX), !is.na(LOCATIONY),
         !is.na(RECIPIENT_WYID)) %>%
  mutate(
    x = as.numeric(LOCATIONX),
    y = as.numeric(LOCATIONY),
    zx = pmin(floor(x / (100 / nx_hot)) + 1, nx_hot),
    zy = pmin(floor(y / (100 / ny_hot)) + 1, ny_hot),
    zone = paste0("z", zx, "_", zy)
  ) %>%
  left_join(
    mdp_div1 %>% rename(RECIPIENT_WYID = PLAYER_WYID, recipient_team = TEAM_WYID),
    by = c("MATCH_WYID", "RECIPIENT_WYID")
  ) %>%
  mutate(accurate = as.integer(!is.na(recipient_team) & recipient_team == TEAM_WYID))

cat("\n1. division passes:", nrow(passes_full_div1), "\n")
cat("Recipient-team found:", sum(!is.na(passes_full_div1$recipient_team)), "/", nrow(passes_full_div1), "\n")
cat("Accuracy rate overall:", round(mean(passes_full_div1$accurate, na.rm = TRUE), 3), "\n")

# -----------------------
# D) Scout: spiller-performance i FCN hot zones (1. division)
# -----------------------
min_hot <- 50

div1_player_hot <- passes_full_div1 %>%
  semi_join(hot_zones %>% select(zone), by = "zone") %>%
  group_by(TEAM_WYID, PLAYER_WYID) %>%
  summarise(n_hot = n(), acc_hot = mean(accurate), .groups = "drop") %>%
  filter(n_hot >= min_hot)

div1_player_all <- passes_full_div1 %>%
  group_by(TEAM_WYID, PLAYER_WYID) %>%
  summarise(n_all = n(), acc_all = mean(accurate), .groups = "drop")

div1_scout <- div1_player_hot %>%
  left_join(div1_player_all, by = c("TEAM_WYID","PLAYER_WYID")) %>%
  mutate(hot_share = n_hot / n_all) %>%
  select(TEAM_WYID, PLAYER_WYID, n_hot, n_all, hot_share, acc_hot, acc_all) %>%
  arrange(desc(acc_hot), desc(hot_share), desc(n_hot))

cat("\nTop 10 1. division spillere i FCN hot zones (min_hot =", min_hot, "):\n")
print(head(div1_scout, 10))

# -----------------------
# E) Tilføj navne (teams + players) – til tabel og evt. senere brug
# -----------------------
players <- dbGetQuery(con, "
  SELECT PLAYER_WYID, FIRSTNAME, LASTNAME
  FROM wyscout_players;
") %>%
  mutate(PLAYER_WYID = as.integer(PLAYER_WYID)) %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>%
  mutate(player_name = paste(FIRSTNAME, LASTNAME)) %>%
  select(PLAYER_WYID, player_name)

teams <- dbGetQuery(con, "
  SELECT TEAM_WYID, TEAMNAME, OFFICIALNAME
  FROM wyscout_teams;
") %>%
  mutate(TEAM_WYID = as.integer(TEAM_WYID)) %>%
  distinct(TEAM_WYID, .keep_all = TRUE) %>%
  mutate(team_name = ifelse(!is.na(OFFICIALNAME) & OFFICIALNAME != "", OFFICIALNAME, TEAMNAME)) %>%
  select(TEAM_WYID, team_name)

div1_scout_named <- div1_scout %>%
  left_join(players, by = "PLAYER_WYID") %>%
  left_join(teams, by = "TEAM_WYID") %>%
  select(team_name, player_name, TEAM_WYID, PLAYER_WYID, n_hot, n_all, hot_share, acc_hot, acc_all)

# -----------------------
# F) Clustering på 1. division spillere (k=3) + PCA plot (uden labels)
# -----------------------
set.seed(1)
k <- 3

df_plot <- div1_scout_named %>%
  filter(n_hot >= min_hot) %>%
  mutate(n_hot_log = log1p(n_hot))

X_p <- scale(df_plot %>% select(acc_hot, hot_share, n_hot_log, acc_all))
km_p <- kmeans(X_p, centers = k, nstart = 50)
df_plot$cluster <- factor(km_p$cluster)

pca <- prcomp(X_p)
df_plot$PC1 <- pca$x[, 1]
df_plot$PC2 <- pca$x[, 2]

var_expl <- (pca$sdev^2) / sum(pca$sdev^2)

# centroids
centroids <- df_plot %>%
  group_by(cluster) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

ggplot(df_plot, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.85, size = 2.2) +
  geom_point(data = centroids, aes(PC1, PC2), size = 5, stroke = 1.2, shape = 4, inherit.aes = FALSE) +
  labs(
    title = paste0("Cluster 2 er klart bedst fit for FCN"),
    subtitle = paste0(
      "X=PC1 (", round(100*var_expl[1],1), "%): samlet fit (acc_hot, hot_share, volumen, acc_all). ",
      "Y=PC2 (", round(100*var_expl[2],1), "%): præcision vs. zone/volumen."
    ),
    x = "PC1 (samlet FCN-fit styrke)",
    y = "PC2 (præcision ↕ vs. zonetilstedeværelse/volumen ↕)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

