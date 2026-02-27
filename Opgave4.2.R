Opgave 4.2 : 
  library(shiny)
library(dplyr)
library(ggplot2)

############################################################
# 1) DATA: FCN-KAMPE + AFLEVERINGER
############################################################

fcn_id <- 7458

# FCN-kampe i Superliga (fra md_fixed i bid 1)
fcn_matches <- md_fixed %>%
  filter(TEAM_WYID == fcn_id) %>%
  distinct(MATCH_WYID, SEASON_WYID)

# Matchlabels fra df_oversigt
match_choices <- df_oversigt %>%
  semi_join(fcn_matches, by = "MATCH_WYID") %>%
  distinct(MATCH_WYID, MATCHLABEL, SEASON_WYID) %>%
  mutate(
    LABEL = paste0(SEASON_WYID, " – ", MATCHLABEL)
  ) %>%
  arrange(LABEL)

# Markér succes/fejl
all_passes_marked <- all_passes %>%
  mutate(
    success = EVENT_WYID %in% passes_succes_super$EVENT_WYID,
    outcome = ifelse(success, "Ramte", "Fejl")
  )

# Filtrér til FCN-kampe
passes_fcn_matches <- all_passes_marked %>%
  semi_join(fcn_matches, by = "MATCH_WYID") %>%
  left_join(teams, by = "TEAM_WYID")   # giver TEAMNAME

# Aggreger afleveringer pr. kamp, hold og outcome
passes_summary <- passes_fcn_matches %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME, outcome) %>%
  summarise(n = n(), .groups = "drop")

############################################################
# 2) UI
############################################################

ui <- fluidPage(
  titlePanel("Opgave 4.2 – Afleveringer i FCN-kampe"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "match_id",
        "Vælg FCN-kamp:",
        choices = setNames(
          match_choices$MATCH_WYID,
          match_choices$LABEL
        )
      )
    ),
    mainPanel(
      plotOutput("passes_plot", height = "600px")
    )
  )
)

############################################################
# 3) SERVER
############################################################

server <- function(input, output, session) {
  
  match_passes <- reactive({
    passes_summary %>%
      filter(MATCH_WYID == input$match_id) %>%
      group_by(TEAMNAME) %>%
      mutate(
        pct = n / sum(n) * 100,
        is_fcn = ifelse(TEAM_WYID == fcn_id, "FCN", "Modstander")
      )
  })
  
  output$passes_plot <- renderPlot({
    df <- match_passes()
    
    ggplot(df, aes(x = TEAMNAME, y = pct, fill = outcome)) +
      geom_col(
        aes(alpha = is_fcn),
        position = position_dodge(width = 0.8),
        color = "black",
        width = 0.7
      ) +
      
      scale_fill_manual(
        values = c(
          "Ramte" = "#1f78b4",
          "Fejl"  = "#e31a1c"
        ),
        labels = c(
          "Ramte" = "Succesfulde afleveringer",
          "Fejl"  = "Fejlafleveringer"
        )
      ) +
      
      scale_alpha_manual(values = c("FCN" = 1, "Modstander" = 0.4)) +
      
      labs(
        title = "Fordeling af afleveringer – succes vs. fejl (i %)",
        subtitle = paste(
          "Kamp:",
          match_choices$MATCHLABEL[match_choices$MATCH_WYID == input$match_id]   # <-- RETTET
        ),
        x = "Hold",
        y = "Procent (%)",
        fill = "Afleveringstype",
        caption = "Rød = fejlafleveringer · Blå = succesfulde afleveringer · FCN = fuld farve · Modstander = dæmpet"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(
          face = ifelse(df$is_fcn == "FCN", "bold", "plain")
        ),
        plot.caption = element_text(size = 11, hjust = 0)
      )
  })
}

############################################################
# 4) RUN APP
############################################################

shinyApp(ui, server)