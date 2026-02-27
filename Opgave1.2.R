###########################

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
install.packages("patchwork")

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



matches <- dbGetQuery(con, "SELECT MATCH_WYID, SEASON_WYID
FROM wyscout_matches
WHERE competition_wyid IN (335, 328)
  AND date BETWEEN '2024-07-19' AND CURRENT_DATE;")

passes <- dbGetQuery(con, "select * from wyscout_matchevents_passes;")
common <- dbGetQuery(con, "SELECT * FROM superliga2.wyscout_matchevents_common;")

### TEAMS ###
teams <- dbGetQuery(con, "
  SELECT TEAM_WYID, TEAMNAME
  FROM wyscout_teams;
") %>%
  distinct(TEAM_WYID, .keep_all = TRUE)


passes_season <- left_join(matches, passes, by="MATCH_WYID")
passes_med_xy <- left_join(passes_season, common[, c(3,14,16,12,13)] %>% distinct(EVENT_WYID, .keep_all = TRUE),by = "EVENT_WYID")
passes_med_xy <- passes_med_xy[,c(1,2,4,3,13,14,5,6,7,8,9,10,15,16,11,12)]

#Kun succesfulde afleveringer
teamz <- unique(na.omit(passes_med_xy$TEAM_WYID[passes_med_xy$SEASON_WYID %in% c(191611,189918)]))
filtered_df <- c()
for(i in 1:length(teamz)){
  filtered_df2 <- passes_med_xy %>% filter(TEAM_WYID==teamz[i])
  filtered_df1 <- filtered_df2 %>% filter(RECIPIENT_WYID %in% unique(filtered_df2$PLAYER_WYID))
  
  filtered_df <- rbind(filtered_df, filtered_df1)
}
passes_succes_super <- filtered_df %>% filter(SEASON_WYID %in% c(191611,189918), PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)

names(passes_season)
names(passes_succes_super)
head(passes_season)
head(passes_succes_super)
str(passes_season)
str(passes_succes_super)

###########################################3
seasons_tokeep <- c(191611, 189918)

all_passes <- passes_med_xy %>%
  filter(SEASON_WYID %in% seasons_tokeep,
         PRIMARYTYPE == "pass",
         PLAYER_WYID != 0,
         RECIPIENT_WYID != 0)

##################### ANTAL AFLEVERINGER ##############
fcn_all_passes_ <- passes_med_xy %>%
  filter(SEASON_WYID %in% seasons_tokeep,
         TEAM_WYID == fcn,
         PLAYER_WYID != 0,
         RECIPIENT_WYID != 0)

fcn_sucess_pass <- passes_succes_super %>%
  filter(TEAM_WYID == fcn)

##### SUCESS RATEN ######
### FCN ###
total_fcn <- nrow(fcn_all_passes_)
sucess_fcn <- nrow(fcn_sucess_pass)
fail_fcn <- total_fcn - sucess_fcn

percent_success <- sucess_fcn /total_fcn *100
percent_fail <- fail_fcn / total_fcn *100  

fcn_o <- tibble(
  kategori = c("Succesfulde afleveringer", "Ikke-succesfulde afleveringer"),
  antal = c(sucess_fcn, fail_fcn),
  procent = c(percent_success, percent_fail),
  total_afleveringer = total_fcn
)

#### SUPERLIGAEN ######
total_sl <- nrow(all_passes)
success_sl <- nrow(passes_succes_super)  
fail_sl <- total_sl - success_sl

percent_success_sl <- success_sl/total_sl * 100
percent_fail_sl <- fail_sl / total_sl *100  

sl_o <- tibble(
  kategori = c("Succesfulde afleveringer", "Ikke-succesfulde afleveringer"),
  antal = c(success_sl, fail_sl),
  procent = c(percent_success_sl, percent_fail_sl),
  total_afleveringer = total_sl
)

########## GENNEMSNIT LÆNGDE ######
avg_length_superliga <- all_passes %>% 
  summarise(avg_length = mean(LENGTH, na.rm = TRUE))

avg_length_succes_super <- passes_succes_super %>% 
  summarise(avg_length = mean(LENGTH, na.rm = TRUE))

avg_length_fcn <- fcn_all_passes_ %>% 
  summarise(avg_length = mean(LENGTH, na.rm = TRUE))

avg_length_fcn_success <- fcn_sucess_pass %>% 
  summarise(avg_length = mean(LENGTH, na.rm = TRUE))

avg_lengths <- tibble(
  kategori = c(
    "Superliga – alle",
    "Superliga – succes",
    "FCN – alle",
    "FCN – succes"
  ),
  avg_length = c(
    avg_length_superliga$avg_length,
    avg_length_succes_super$avg_length,
    avg_length_fcn$avg_length,
    avg_length_fcn_success$avg_length
  ),
  y = c(80, 60, 40, 20)   # placeringer på banen
)


########################### PLOT #####################3
ggplot(avg_lengths) +
  annotate_pitch(colour = "black", fill = "white") +
  
  geom_segment(
    aes(
      x = 5,                    # starter lidt inde på banen
      y = y,
      xend = 5 + avg_length,
      yend = y,
      colour = kategori
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 2.5
  ) +
  
  geom_text(
    aes(
      x = 5 + avg_length,
      y = y + 3,
      label = paste0(round(avg_length, 1), " m"),
      colour = kategori
    ),
    size = 6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_colour_viridis_d() +
  coord_fixed() +   # 👈 ingen xlim!
  
  labs(
    title = "Gennemsnitlig længde på afleveringer",
    subtitle = "FC Nordsjælland vs. Superligaen",
    colour = "Kategori",
    caption = "Kilde: Wyscout"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )

####### POSITION PÅ BANEN DER MODTAGER FLEST AFLEVERINGER #####

sl_most_common_zone <- all_passes %>% 
  mutate(zone = paste0(round(ENDLOCATIONX/10)*10), "-", round(ENDLOCATIONY/10)*10) %>% 
  count(zone, sort = TRUE)

fcn_most_common_zone <- fcn_all_passes_%>%
  mutate(zone = paste0(round(ENDLOCATIONX/10)*10, "-", round(ENDLOCATIONY/10)*10)) %>% 
  count(zone, sort = TRUE)

### TOP 3 zoner for flest afleveringer ###

########## SUPERLIGA #########
sl_zones <- all_passes %>%
  mutate(
    zone_x = round(ENDLOCATIONX/10)*10,
    zone_y = round(ENDLOCATIONY/10)*10
  ) %>%
  count(zone_x, zone_y, name = "n")
sl_total <- sum(sl_zones$n)

sl_top3 <- sl_zones %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%
  mutate(pct = n / sl_total * 100)

########## FCN #########################
fcn_zones <- fcn_all_passes_ %>%
  mutate(
    zone_x = round(ENDLOCATIONX/10)*10,
    zone_y = round(ENDLOCATIONY/10)*10
  ) %>%
  count(zone_x, zone_y, name = "n")

fcn_total <- sum(fcn_zones$n)

fcn_top3 <- fcn_zones %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%
  mutate(pct = n / fcn_total * 100)

##########
library(ggplot2)
library(ggsoccer)

p_sl <- ggplot() +
  annotate_pitch(colour = "black", fill = "white") +
  geom_tile(
    data = sl_top3,
    aes(x = zone_x, y = zone_y, fill = pct),
    alpha = 0.85
  ) +
  scale_fill_viridis_c(option = "C") +
  geom_text(
    data = sl_top3,
    aes(x = zone_x, y = zone_y, label = sprintf("%.1f%%", pct)),
    color = "white", size = 6, fontface = "bold"
  ) +
  coord_fixed() +
  labs(
    title = "Superligaen – Top 3 afleveringszoner",
    subtitle = "Procent af alle afleveringer",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_blank())

p_fcn <- ggplot() +
  annotate_pitch(colour = "black", fill = "white") +
  geom_tile(
    data = fcn_top3,
    aes(x = zone_x, y = zone_y, fill = pct),
    alpha = 0.85
  ) +
  scale_fill_viridis_c(option = "C") +
  geom_text(
    data = fcn_top3,
    aes(x = zone_x, y = zone_y, label = sprintf("%.1f%%", pct)),
    color = "white", size = 6, fontface = "bold"
  ) +
  coord_fixed() +
  labs( 
    title = "FC Nordsjælland – Top 3 afleveringszoner",
    subtitle = "Procent af alle afleveringer",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_blank())

#######
library(patchwork)

p_sl + p_fcn


############# ANDEL AF AFLEVERINGER DER BLIVER TIL SKUD #######

##### Sortér alle events i rækkefølge #####3
event_sorted <- common %>% 
  filter(SEASON_WYID %in% seasons_tokeep) %>% 
  arrange(MATCH_WYID,EVENT_WYID)

###### 
events_with_next <- event_sorted %>% 
  mutate(
    next_team = lead(TEAM_WYID),
    next_type = lead(PRIMARYTYPE)
  )

############ FILTRÉR AFLEVERINGER HVOR NÆSTE EVENT ER SKUD ####

passes_to_shots <- events_with_next %>% 
  filter(PRIMARYTYPE == "pass",
         next_type == "shot",
         TEAM_WYID == next_team
  )
fcn_passes_two <- fcn_all_passes_ %>% 
  filter(SEASON_WYID %in% seasons_tokeep)

sl_passes_two <- all_passes %>% 
  filter(SEASON_WYID %in% seasons_tokeep)

########### FCN ANDEL DER BLIVER TIL SKUD ########

percent_fcn_passes_to_shots <- passes_to_shots %>% 
  filter(TEAM_WYID == fcn) %>% 
  nrow()/nrow(fcn_passes_two)*100

########### SUPERLIGA ANDEL DER BLIVER TIL SKUD ########

percent_passes_to_shots <- nrow(passes_to_shots) / nrow(sl_passes_two)*100

passes_to_shots_tbl <- tibble(
  hold = c("FCN", "Superligaen"),
  passes_to_shots = c(
    passes_to_shots %>% filter(TEAM_WYID == fcn) %>% nrow(),
    nrow(passes_to_shots)
  ),
  total_passes = c(
    nrow(fcn_all_passes_),
    nrow(all_passes)
  ),
  percent_passes_to_shots = c(
    percent_fcn_passes_to_shots,
    percent_passes_to_shots
  )
)

######### HVILKE HOLD ER DE DÅRLIGSTE TIL AT AFLEVERE PRÆCIST? ######
### succesrate pr. hold ###
pass_stats <- all_passes %>%
  group_by(TEAM_WYID) %>%
  summarise(
    total = n(),
    succes = sum(EVENT_WYID %in% passes_succes_super$EVENT_WYID),
    succes_rate = succes / total * 100
  ) %>%
  arrange(succes_rate)

pass_stats_named <- pass_stats %>%
  left_join(teams, by = "TEAM_WYID") %>%
  select(TEAM_WYID, TEAMNAME, total, succes, succes_rate) %>%
  arrange(succes_rate)


############### TOP 3 DÅRLIGE ##########
bottom3 <- pass_stats_named %>% slice_head(n = 3)

############## TOP 3 #####################
top3 <- pass_stats_named %>%
  arrange(desc(succes_rate)) %>%
  slice_head(n = 3)


caption = "Kilde: Wyscout"

################# Afleveringer i sidste tredjedel af banen ###########
### Wyscout skalerer banen 0-100 i X-retningen

### FCN ###
fcn_last_third <- fcn_all_passes_ %>% 
  filter(LOCATIONX > 66.7)

fcn_last_third_success <- fcn_sucess_pass %>% 
  filter(LOCATIONX > 66.7)

fcn_total_last_third <- nrow(fcn_last_third)
fcn_success_last_third <- nrow(fcn_last_third_success)
fcn_success_rate_last_third <- fcn_success_last_third / fcn_total_last_third * 100

### Superliga ###
last_third_all <- all_passes %>% 
  filter(LOCATIONX > 66.7)

last_third_success <- passes_succes_super %>% 
  filter(LOCATIONX > 66.7)

sl_total_last_third <- nrow(last_third_all)
sl_success_last_third <- nrow(last_third_success)
sl_success_rate_last_third <- sl_success_last_third / sl_total_last_third * 100

### Tabel ###
last_third_tbl <- tibble(
  hold = c("FCN", "Superligaen"),
  total = c(fcn_total_last_third, sl_total_last_third),
  succes = c(fcn_success_last_third, sl_success_last_third),
  succes_rate = c(fcn_success_rate_last_third, sl_success_rate_last_third)
)

library(ggplot2)

ggplot(last_third_tbl, aes(x = hold, y = succes_rate, fill = hold)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("FCN" = "red", "Superligaen" = "grey40")) +
  labs(
    title = "Succesrate for afleveringer i sidste tredjedel",
    x = "",
    y = "Succesrate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Alle afleveringer i sidste tredjedel
sl_last_third <- all_passes %>%
  filter(LOCATIONX > 66.7)

# Succesfulde afleveringer i sidste tredjedel
sl_last_third_success <- passes_succes_super %>%
  filter(LOCATIONX > 66.7)

# Lav et hurtigt lookup-set for hurtigere matching
success_ids_last_third <- sl_last_third_success$EVENT_WYID

# Beregn succesrate pr. hold
last_third_stats <- sl_last_third %>%
  group_by(TEAM_WYID) %>%
  summarise(
    total = n(),
    succes = sum(EVENT_WYID %in% success_ids_last_third),
    succes_rate = succes / total * 100
  )
teams_unique <- teams %>% distinct(TEAM_WYID, .keep_all = TRUE)

last_third_stats_named <- last_third_stats %>%
  left_join(teams_unique, by = "TEAM_WYID") %>%
  select(TEAM_WYID, TEAMNAME, total, succes, succes_rate) %>%
  arrange(desc(succes_rate))

top3_last_third <- last_third_stats_named %>% slice_head(n = 3)
top3_last_third

########################################################################################

ggplot(top3_last_third, aes(x = reorder(TEAMNAME, succes_rate), y = succes_rate, fill = succes_rate)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", succes_rate)),
            hjust = -0.1, size = 5, color = "black") +
  scale_fill_gradient(low = "orange", high = "darkgreen") +
  coord_flip() +
  expand_limits(y = max(top3_last_third$succes_rate) + 5) +
  labs(
    title = "TFC Nordsjælland topper ligaen i afleveringssucces i sidste tredjedel",
    subtitle = "Succesrate for afleveringer i sidste tredjedel af banen",
    x = "Hold",
    y = "Succesrate (%)",
    caption = "Kilde: Wyscout"
  ) +
  theme_light(base_size = 14) +
  theme(legend.position = "none")

# Filtrer kun afleveringer og find de 3 dårligste hold
all_passes_clean <- passes_med_xy %>%
  filter(
    SEASON_WYID %in% c(191611, 189918),
    PRIMARYTYPE == "pass",
    PLAYER_WYID != 0,
    RECIPIENT_WYID != 0
  )

# Beregn succesrate pr. hold
pass_stats <- all_passes_clean %>%
  group_by(TEAM_WYID) %>%
  summarise(
    total = n(),
    succes = sum(ACCURATE == 1, na.rm = TRUE),
    succes_rate = succes / total * 100,
    .groups = "drop"
  )

ggplot(bottom3,
       aes(x = reorder(TEAMNAME, succes_rate),
           y = succes_rate,
           fill = succes_rate)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", succes_rate)),
            hjust = -0.1,
            size = 5) +
  coord_flip() +
  scale_fill_gradient(low = "firebrick", high = "orange") +
  expand_limits(y = max(bottom3$succes_rate) + 5) +
  labs(
    title = "De 3 hold med lavest afleveringspræcision",
    subtitle = "Samlet succesrate for afleveringer",
    x = NULL,
    y = "Succesrate (%)",
    caption = "Kilde: Wyscout"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

