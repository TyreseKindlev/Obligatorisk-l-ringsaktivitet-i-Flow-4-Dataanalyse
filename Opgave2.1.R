####### OLA OPGAVE 2.1 #########################

########### Nordsjælland 2.0  #########
library(RMariaDB)
library(dplyr)
library(ggplot2)
library(stringr)
library(rpart)
library(tidyr)
library(ggsoccer)
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

##### Nordsjælland ID 
fcn <- 7458
##test
dbListTables(con)
query1 <- " SELECT * FROM wyscout_matches";
df_oversigt <- dbGetQuery(con, query1)


######################################
######################################

######### MÅLSCORE OG SÆSON #########

md <- dbGetQuery(con, "
  SELECT 
    mdb.MATCH_WYID,
    mdb.TEAM_WYID,
    mdb.SCORE,
    m.SEASON_WYID,
    m.COMPETITION_WYID
  FROM wyscout_matchdetail_base AS mdb
  LEFT JOIN wyscout_matches AS m
    ON mdb.MATCH_WYID = m.MATCH_WYID
  WHERE m.COMPETITION_WYID IN (335, 328)
    AND m.SEASON_WYID IN (191611, 189918);
")

########## SPLIT SCORE TIL MÅL FOR OG IMOD #####
library(dplyr)
library(tidyr)

md_clean <- md %>%
  filter(!is.na(SCORE), SCORE != "") %>% 
  separate(SCORE, into = c("goals_for", "goals_against"), sep = "-", convert = TRUE)

md_fixed <- md_clean %>%
  select(MATCH_WYID, TEAM_WYID, goals_for, SEASON_WYID) %>%
  left_join(
    md_clean %>%
      select(MATCH_WYID, TEAM_WYID, opp_goals_for = goals_for),
    by = "MATCH_WYID"
  ) %>%
  filter(TEAM_WYID.x != TEAM_WYID.y) %>%   # fjern join med sig selv
  rename(
    TEAM_WYID = TEAM_WYID.x,
    goals_for = goals_for,
    goals_against = opp_goals_for
  ) %>%
  select(MATCH_WYID, TEAM_WYID, goals_for, goals_against, SEASON_WYID)


########## BEREGN POINT ################
md_points <- md_fixed %>% 
  mutate(
    points = case_when(
      goals_for > goals_against ~3,
      goals_for == goals_against ~1,
      TRUE ~0
    )
  )



########## LAV STANDINGS (TOP 5 / BUND 5)
standings <- md_points %>% 
  group_by(SEASON_WYID, TEAM_WYID) %>% 
  summarise(
    points = sum(points),
    goals_for = sum(goals_for),
    goals_against = sum(goals_against),
    goal_diff = goals_for - goals_against
  ) %>% 
  arrange(SEASON_WYID, desc(points), desc(goal_diff), desc(goals_for)) %>% 
  group_by(SEASON_WYID) %>% 
  mutate(rank = row_number())


######## DEFINÉR TOP 5 / BUND 5 #######
top_bottom <-  standings %>% 
  mutate(group = ifelse(rank <= 5, "Top 5", "Bund 5"))



######## JOIN MED AFLEVERINGER ############
passes_tb <- all_passes %>% 
  left_join(top_bottom, by = c("TEAM_WYID", "SEASON_WYID"))

############# AFLEVERINGSSTATISTIK FOR TOP 5 VS BUND 5 ###########
tb_summary <- passes_tb %>%
  group_by(SEASON_WYID, group) %>%
  summarise(
    total_passes = n(),
    avg_passes_per_match = n() / n_distinct(MATCH_WYID),
    avg_length = mean(LENGTH, na.rm = TRUE),
    success_rate = mean(EVENT_WYID %in% passes_succes_super$EVENT_WYID) * 100
  )
View(tb_summary)  


############ PLOT ###################
library(ggplot2)
ggplot(tb_summary, aes(x = group, y = success_rate, fill = group)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%", success_rate)),
    vjust = -0.5,
    size = 5
  ) +
  facet_wrap(~ SEASON_WYID) +
  labs(
    title = "Succesrate for afleveringer – Top 5 vs Bund 5",
    x = "",
    y = "Succesrate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



################## AFLEVERINGER I 80 MINUT SAMMENLIGNET ###########

## sæsoner 
seasons_tokeep <- c(191611,189918)

##### HENT minutdata fra common 
minuts <- dbGetQuery(con, "
  SELECT EVENT_WYID, MINUTE
  FROM wyscout_matchevents_common
")

##### join afleveringer med minutdata 
passes_time <- all_passes %>% 
  left_join(minuts, by = "EVENT_WYID") %>% 
  filter(!is.na(MINUTE)) %>% 
  mutate(period = ifelse(MINUTE >= 80, "80+", "0-79"))

#### Markér sucessfulde afleveringer korrekt 
passes_time<- passes_time %>% 
  mutate(success = EVENT_WYID %in% passes_succes_super$EVENT_WYID)

###### summary 
passes_time_summary <- passes_time %>% 
  group_by(SEASON_WYID, period) %>% 
  summarise(
    total_passes = n(),
    success_passes = sum(success),
    success_rate = success_passes / total_passes *100
  )



###################### PLOT #######################
library(dplyr)
library(ggplot2)

passes_pct <- passes_time_summary %>%
  group_by(SEASON_WYID) %>%
  mutate(share_pct = total_passes / sum(total_passes) * 100)

ggplot(passes_pct, aes(x = period, y = share_pct, fill = period)) +
  geom_col() +
  facet_wrap(~ SEASON_WYID) +
  labs(
    title = "Mange flere afleveringer i 0-79 min end 80+ minut ",
    x = "Periode",
    y = "Andel af alle afleveringer (%)"
  ) +
  theme_minimal()

passes_pct2 <- passes_time_summary %>%
  group_by(SEASON_WYID) %>%
  mutate(success_pct = success_passes / sum(success_passes) * 100)


ggplot(passes_pct2, aes(x = period, y = success_pct, fill = period)) +
  geom_col() +
  facet_wrap(~ SEASON_WYID) +
  labs(
    title = "Omkring 13% af de succesfulde afleveringer ligger i +80 minut ",
    x = "Periode",
    y = "Andel af succesfulde afleveringer (%)"
  ) +
  theme_minimal()




####### ASIST 2.0 ########
library(stringr)
df_sec <- dbReadTable(con, "wyscout_matchevents_secondarytype")

# Find alle kolonner der starter med "SECONDARYTYPE"
sec_cols <- grep("^SECONDARYTYPE", names(df), value = TRUE)

shot_assist_passes <- df_sec %>%
  left_join(matches %>% select(MATCH_WYID, SEASON_WYID), by = "MATCH_WYID") %>%
  filter(
    PRIMARYTYPE == "pass",
    if_any(all_of(sec_cols), ~ str_detect(.x, regex("shot_assist", ignore_case = TRUE))),
    SEASON_WYID %in% c(191611, 189918)
  )


antal_assist_sec1 <- shot_assist_passes%>% 
  filter(tolower(SECONDARYTYPE1) == "assist") %>% 
  select(EVENT_WYID, MATCH_WYID, PRIMARYTYPE, SECONDARYTYPE1)

assist_passes <- antal_assist_sec1 %>% 
  inner_join(all_passes, by = "EVENT_WYID")

############ BEREGN LÆNGDEN ##############

mean(assist_passes$LENGTH, na.rm = TRUE)

assist_team_fcn <- assist_passes %>% 
  filter(TEAM_WYID == 7458)

mean_lg_fcn <- mean(assist_team_fcn$LENGTH, na.rm = TRUE)

top3_assist_lg <- assist_passes %>% 
  group_by(TEAM_WYID) %>%
  summarise(
    mean_length = mean(LENGTH, na.rm = TRUE),
    n = n()
  ) %>% 
  arrange(desc(mean_length)) %>% 
  slice_head(n = 3)
top3_assist_names <- top3_assist_lg %>% 
  left_join(teams, by = "TEAM_WYID")


bund_assist_length <- assist_passes %>% 
  group_by(TEAM_WYID) %>% 
  summarise(
    mean_length  = mean(LENGTH, na.rm = TRUE),
    n =n()
  ) %>% 
  arrange(mean_length) %>% 
  slice_head(n = 3) %>% 
  left_join(teams, by = "TEAM_WYID")


##### PLOT ########
library(ggplot2)

assist_team_stats <- assist_passes %>% 
  group_by(TEAM_WYID) %>% 
  summarise(
    mean_length = mean(LENGTH, na.rm = TRUE),
    n = n()
  ) %>% 
  left_join(teams, by = "TEAM_WYID") %>% 
  arrange(mean_length)

ggplot(assist_team_stats, 
       aes(x = mean_length, 
           y = reorder(TEAMNAME, mean_length))) +
  
  geom_col(fill = "steelblue", width = 0.7) +
  
  geom_text(aes(label = paste0(round(mean_length, 1), " m")),
            hjust = -0.15,
            size = 4) +
  
  labs(
    title = "Assist-afleveringer er længst hos Lyngby",
    subtitle = "Gennemsnitlig længde på assistet afleveringer i meter | Superliga 24/25 & 25/26",
    x = "Gennemsnitlig længde (meter)",
    y = "Hold",
    caption = "Kilde: Wyscout"
  ) +
  
  expand_limits(x = max(assist_team_stats$mean_length) + 3) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    axis.text.y = element_text(size = 11)
  )




############## Assist vs ikke assist #############
all_passes_marked <- all_passes %>% 
  mutate(
    is_assist = EVENT_WYID %in% assist_passes$EVENT_WYID
  )

############ BEREGN gennemsnit for begge grupper #####
length_compare <- all_passes_marked %>% 
  group_by(is_assist) %>% 
  summarise(
    mean_length = mean(LENGTH, na.rm = TRUE),
    median_length = median(LENGTH, na.rm = TRUE),
    n = n()
  )

###### BEREGNER FORSKELLEN ##########
difference <- length_compare$mean_length[length_compare$is_assist == TRUE] - 
  length_compare$mean_length[length_compare$is_assist == FALSE]


library(ggplot2)

ggplot(length_compare, aes(x = is_assist, 
                           y = mean_length, 
                           fill = is_assist)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = paste0(round(mean_length, 1), " m")),
            vjust = -0.5,
            size = 5) +
  
  labs(
    title = "Målskabende afleveringer slås fra en lidt længere afstand",
    x = "Assist (TRUE) vs. Ikke assist (FALSE)",
    y = "Gennemsnitlig længde (meter)",
    caption = "Kilde: Wyscout"
  ) +
  
  scale_fill_manual(values = c("grey70", "steelblue")) +
  
  coord_cartesian(ylim = c(0, 21)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, face = "italic")
  )


####### FCN ASSIST #######
passes_fcn <- all_passes_marked %>% 
  filter(TEAM_WYID == 7458)

length_compare_fcn <- passes_fcn %>% 
  group_by(is_assist) %>% 
  summarise(mean_length = mean(LENGTH, na.rm = TRUE))

ggplot(length_compare_fcn, 
       aes(x = is_assist, 
           y = mean_length, 
           fill = is_assist)) +
  
  geom_col(width = 0.6) +
  
  geom_text(aes(label = paste0(round(mean_length, 1), " m")),
            vjust = -0.5,
            size = 5) +
  
  labs(
    title = "FC Nordsjællands assist-afleveringer er længere end gennemsnit",
    x = "Assist (TRUE) vs. Ikke assist (FALSE)",
    y = "Gennemsnitlig længde (meter)",
    caption = "Kilde: Wyscout"
  ) +
  
  scale_fill_manual(values = c("grey70", "steelblue")) +
  
  coord_cartesian(ylim = c(0, 21)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, face = "italic")
  )



###### HVOR kommer assist fra?? 
wyscout_positions <- tbl(con, "wyscout_players") %>%
  select(player_id = PLAYER_WYID, role = ROLENAME, shortname = SHORTNAME) %>%
  collect() %>%   # <-- HENT DATA FØRST
  mutate(
    line = case_when(
      grepl("Defend", role, ignore.case = TRUE) ~ "Forsvar",
      grepl("Midfield", role, ignore.case = TRUE) ~ "Midtbane",
      grepl("Forward|Striker|Wing", role, ignore.case = TRUE) ~ "Angreb",
      TRUE ~ "Ukendt"
    )
  )

###### merge med afleveringer ##### 

passes_with_pos <- all_passes_marked %>%
  left_join(wyscout_positions, by = c("PLAYER_WYID" = "player_id"))

passes_with_pos_clean <- passes_with_pos %>%
  filter(!is.na(line), line != "Ukendt")

######## Beregn andelen af assists pr. linje 
assist_share <- passes_with_pos_clean %>% 
  group_by(line) %>% 
  summarise(
    total_passes = n(),
    total_assist = sum(is_assist, na.rm = TRUE),
    assist_rate = total_assist/total_passes
  )

########### FCN POSITION ######
passes_fcn_with_pos <- passes_fcn %>% 
  left_join(wyscout_positions, by = c("PLAYER_WYID"= "player_id"))

###### beregn assist andel 
assist_share_fcn <- passes_fcn_with_pos %>%
  filter(!is.na(line), line != "Ukendt") %>%
  group_by(line) %>%
  summarise(
    total_passes = n(),
    total_assists = sum(is_assist, na.rm = TRUE),
    assist_rate = total_assists / total_passes
  )



######## mål events til links af assist #####
matches_two <- all_passes_marked %>% 
  distinct(MATCH_WYID)

assist_distribution <- passes_with_pos_clean %>%
  filter(is_assist == TRUE) %>%
  group_by(line) %>%
  summarise(
    assists = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share = assists / sum(assists)
  )

ggplot(assist_distribution, 
       aes(x = reorder(line, share), 
           y = share, 
           fill = line)) +
  
  geom_col(width = 0.7) +
  
  geom_text(aes(label = paste0(round(share*100, 1), "%")),
            vjust = -0.5,
            size = 5) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.6)) +
  
  labs(
    title = "Midtbane og angreb står for størstedelen af assist i ligaen",
    subtitle = "Fordeling af assist på forsvar, midtbane og angreb | Superliga 24/25 & 25/26",
    x = "Position",
    y = "Andel af samlede assist",
    caption = "Kilde: Wyscout"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic")
  )


#####################
mdp <- tbl(con, "wyscout_matchdetail_players") %>%
  filter(MATCH_WYID %in% matches_two$MATCH_WYID) %>%
  select(PLAYER_WYID, TEAM_WYID, PLAYERASSISTS) %>%
  collect()

###### summeér assist pr. spiller 
assist_totals <- mdp %>%
  group_by(PLAYER_WYID, TEAM_WYID) %>%
  summarise(assists = sum(PLAYERASSISTS, na.rm = TRUE), .groups = "drop")

wyscout_players <- tbl(con, "wyscout_players") %>% collect()
wyscout_teams <- tbl(con, "wyscout_teams") %>% collect()


assist_total_names <- assist_totals %>% 
  left_join(wyscout_players, by = "PLAYER_WYID") %>% 
  left_join(wyscout_teams, by = "TEAM_WYID")

assist_totals_named <- assist_totals %>%
  left_join(wyscout_players, by = "PLAYER_WYID") %>%
  left_join(wyscout_teams,   by = "TEAM_WYID") %>%
  distinct(PLAYER_WYID, TEAM_WYID, .keep_all = TRUE)


##### top 10 
top10_goal_assists <- assist_totals_named %>%
  arrange(desc(assists)) %>%
  slice_head(n = 10) %>%
  select(SHORTNAME, team = TEAMNAME, assists)


ggplot(top10_goal_assists, 
       aes(x = reorder(SHORTNAME, assists), 
           y = assists, 
           fill = team)) +
  
  geom_col(width = 0.7) +
  
  geom_text(aes(label = assists),
            hjust = -0.2,
            size = 4) +
  
  coord_flip() +
  
  scale_y_continuous(limits = c(0, 14), breaks = 0:14) +
  
  labs(
    title = "Top 10 spillere med flest assist i Superliga 24/25 & 25/26",
    subtitle = "S. Nordli topper listen med 13 assist",
    x = "Spiller",
    y = "Antal assist",
    fill = "Hold",
    caption = "Kilde: Wyscout"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic")
  )




