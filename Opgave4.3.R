############################################################
# Opgave 4.3 – Shiny app: Skud i åben spil i en kamp
# (Superliga: 189918 + 191611, filtrerer corn/free/pena,
#  mål defineres via total_goal, coords fra common, holdnavn fra teams)
############################################################

library(DBI)
library(RMariaDB)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(shiny)
library(ggsoccer)

con <- dbConnect(
  MariaDB(),
  host     = "www.talmedos.com",
  port     = 3306,
  user     = "dalremote",
  password = "OttoRehagel123456789Long2026!",
  dbname   = "superliga2"
)

valid_seasons <- c(189918, 191611)

############################################################
# 1) HENT MATCH-OVERSIGT (til dropdown)
############################################################
df_oversigt <- dbGetQuery(con, "
SELECT MATCH_WYID, SEASON_WYID, MATCHLABEL, COMPETITION_WYID
FROM wyscout_matches
WHERE COMPETITION_WYID = 335
  AND SEASON_WYID IN (189918, 191611);
")

############################################################
# 2) HENT SHOTS (de to sæsoner) + fjern dubletter på EVENT_WYID
############################################################
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

shots_all <- bind_rows(shots_24_25, shots_25_26)

############################################################
# 3) FILTRÉR TIL ÅBENT SPIL + BYG total_goal (0/1)
############################################################
shots_cpf <- shots_all %>%
  filter(!PRIMARYTYPE %in% c("corn", "free", "pena")) %>%
  mutate(
    goal_from_shotisgoal = as.integer(SHOTISGOAL == 1),
    goal_from_bodypart   = as.integer(str_detect(tolower(as.character(SHOTBODYPART)), "goal")),
    total_goal           = as.integer(goal_from_shotisgoal == 1 | goal_from_bodypart == 1)
  )

############################################################
# 4) HENT COORDS FRA COMMON + HOLDNAVN FRA TEAMS
############################################################
common_xy <- dbGetQuery(con, "
SELECT EVENT_WYID, MATCH_WYID, TEAM_WYID, LOCATIONX, LOCATIONY
FROM wyscout_matchevents_common
WHERE COMPETITION_WYID = 335
  AND SEASON_WYID IN (189918, 191611);
") %>% distinct(EVENT_WYID, .keep_all = TRUE)

teams <- dbGetQuery(con, "
SELECT TEAM_WYID, TEAMNAME
FROM wyscout_teams
WHERE COMPETITION_WYID = 335
  AND SEASON_WYID IN (189918, 191611);
") %>% distinct(TEAM_WYID, .keep_all = TRUE)

tmp <- shots_cpf %>%
  mutate(EVENT_WYID = as.character(EVENT_WYID)) %>%
  left_join(common_xy %>% mutate(EVENT_WYID = as.character(EVENT_WYID)), by = "EVENT_WYID") %>%
  left_join(teams, by = "TEAM_WYID") %>%
  mutate(
    x = LOCATIONX,
    y = LOCATIONY,
    is_goal = ifelse(total_goal == 1, "Mål", "Ikke mål")
  )

# Saml match-id til én kolonne (pga .x/.y/.1 fra join)
shots_plot <- tmp %>%
  mutate(
    MATCH_WYID = coalesce(MATCH_WYID.x, MATCH_WYID.y, MATCH_WYID.1),
    MATCH_WYID = as.integer(MATCH_WYID)
  ) %>%
  select(EVENT_WYID, MATCH_WYID, SEASON_WYID, TEAM_WYID, TEAMNAME, x, y, is_goal, total_goal, SHOTXG, SHOTPOSTSHOTXG)

############################################################
# 5) MATCH_CHOICES (dropdown)
############################################################
goal_count <- shots_cpf %>%
  group_by(MATCH_WYID) %>%
  summarise(goals_in_shots = sum(total_goal == 1, na.rm = TRUE), .groups = "drop")

match_choices <- df_oversigt %>%
  filter(SEASON_WYID %in% valid_seasons) %>%
  left_join(goal_count, by = "MATCH_WYID") %>%
  mutate(
    goals_in_shots = ifelse(is.na(goals_in_shots), 0, goals_in_shots),
    label_with_season = paste0(MATCHLABEL, " (", SEASON_WYID, ")")
  ) %>%
  arrange(label_with_season)

############################################################
# 6) SHINY APP
############################################################
ui <- fluidPage(
  titlePanel("Opgave 4.3 – Skud i åben spil i en kamp"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "match_id",
        "Vælg kamp:",
        choices = setNames(match_choices$MATCH_WYID, match_choices$label_with_season)
      )
    ),
    mainPanel(
      plotOutput("shotplot", height = "650px")
    )
  )
)

server <- function(input, output, session) {
  
  kamp_skud <- reactive({
    req(input$match_id)
    shots_plot %>%
      filter(MATCH_WYID == as.integer(input$match_id))
  })
  
  output$shotplot <- renderPlot({
    df <- kamp_skud()
    
    ggplot() +
      annotate_pitch(colour = "grey70", fill = "white") +
      theme_pitch() +
      geom_point(
        data = df,
        aes(
          x = x,
          y = y,
          color = TEAMNAME,
          shape = is_goal,
          size  = is_goal
        ),
        alpha = 0.85
      ) +
      scale_shape_manual(values = c("Ikke mål" = 1, "Mål" = 16)) +
      scale_size_manual(values = c("Ikke mål" = 3, "Mål" = 6)) +
      scale_color_discrete() +
      labs(
        title = "Skud i kampen",
        subtitle = match_choices$MATCHLABEL[match_choices$MATCH_WYID == as.integer(input$match_id)],
        color = "Hold",
        shape = "Mål?",
        size  = "Mål?"
      ) +
      coord_flip() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
  })
}

shinyApp(ui, server)
