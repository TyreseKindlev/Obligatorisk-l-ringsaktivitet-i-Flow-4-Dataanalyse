library(shiny)
library(dplyr)
library(ggplot2)

############################################################
# DATAFORBEREDELSE
############################################################

player_stats <- shots_master %>%
  left_join(players_role, by = "PLAYER_WYID") %>%
  group_by(PLAYER_WYID, player_name) %>%
  summarise(
    goals = sum(total_goal, na.rm = TRUE),
    xg    = sum(SHOTXG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(goals))

############################################################
# UI
############################################################

ui <- fluidPage(
  titlePanel("Top målscorere og xG"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "top_n",
        "Vælg antal spillere:",
        choices = c("10", "20", "30"),   # tekst i UI
        selected = "10"
      )
    ),
    mainPanel(
      plotOutput("top_plot", height = "750px")
    )
  )
)

############################################################
# SERVER
############################################################

server <- function(input, output, session) {
  
  filtered_players <- reactive({
    n <- as.numeric(input$top_n)   # <-- FIX: konverter til tal
    player_stats %>%
      slice_head(n = n)
  })
  
  output$top_plot <- renderPlot({
    df <- filtered_players()
    
    ggplot(df, aes(y = reorder(player_name, goals))) +
      geom_col(aes(x = goals), fill = "black", width = 0.6) +
      geom_point(aes(x = xg), color = "red", size = 4) +
      labs(
        x = "Antal mål",
        y = "Spillere",
        title = paste("Top", input$top_n, "målscorere"),
        subtitle = "Søjler = mål, røde prikker = xG"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 22),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
}

############################################################
# RUN APP
############################################################

shinyApp(ui, server)

names(shots_geo)