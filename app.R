library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(plotly)


# Load data
spotify <- read.csv("data/spotify_tracks.csv")

spotify_clean <- spotify %>%
  mutate(genre = ifelse(genre == "set()", "No Genre", genre)) %>%
  mutate(genre = strsplit(as.character(genre), ",")) %>%
  unnest(genre) %>%
  mutate(genre = str_trim(genre)) %>%
  mutate(explicit = ifelse(explicit == "True", "Explicit", "Clean"))

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .title-panel {
        background-color: #1DB954;
        color: white;
        padding: 15px;
        border-radius: 10px;
      }
    "))
  ),
  
  div(class = "title-panel",
      img(src = "logo.png", height = "60px"),
      h2("Spotify Music Explorer Dashboard")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "genre",
        "Select Genre",
        choices = sort(unique(spotify_clean$genre)),
        multiple = TRUE
      ),
      sliderInput("year", "Year Range",
                  min = min(spotify$year),
                  max = max(spotify$year),
                  value = c(2015, 2023),
                  step = 1),
      sliderInput("pop", "Popularity",
                  min = 0, max = 100, value = c(20, 80))
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Overview",
                 plotOutput("popHist"),
                 plotOutput("trendPlot")
        ),
        
        tabPanel("Compare",
                 plotlyOutput("scatterPlot"),
                 plotOutput("barPlot")
        ),
        
        tabPanel("Features",
                 plotOutput("corPlot")
        ),
        
        tabPanel("Data Explorer",
                 DTOutput("table"),
                 hr(),
                 h4("Selected Song Details"),
                 verbatimTextOutput("songInfo"),
                 plotOutput("songRadar")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    
    spotify_clean %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        popularity >= input$pop[1],
        popularity <= input$pop[2]
      ) %>%
      {
        # If no genre selected → skip genre filter
        if (is.null(input$genre) || length(input$genre) == 0) {
          .
        } else {
          filter(., genre %in% input$genre)
        }
      } %>%
      distinct(song, artist, .keep_all = TRUE)
  })
  
  # 1. Histogram
  output$popHist <- renderPlot({
    ggplot(filtered(), aes(popularity)) +
      geom_histogram(bins = 20, fill = "steelblue")
  })
  
  # 2. Trend
  output$trendPlot <- renderPlot({
    filtered() %>%
      group_by(year) %>%
      summarise(avg_pop = mean(popularity)) %>%
      ggplot(aes(year, avg_pop)) +
      geom_line(color = "darkgreen")
  })
  
  # 3. Scatter
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered(), aes(
      x = danceability,
      y = energy,
      color = explicit,
      text = paste0(
        "Song: ", song,
        "<br>Artist: ", artist,
        "<br>Popularity: ", popularity,
        "<br>Genre: ", genre
      )
    )) +
      geom_point(alpha = 0.8, size = 0.8) +
      scale_color_manual(values = c("Explicit" = "red", "Clean" = "steelblue")) +
      labs(color = "Content Rating")
    
    ggplotly(p, tooltip = "text")
  })
  
  # 4. Bar chart
  output$barPlot <- renderPlot({
    filtered() %>%
      group_by(artist) %>%
      summarise(avg_pop = mean(popularity)) %>%
      top_n(10) %>%
      ggplot(aes(reorder(artist, avg_pop), avg_pop)) +
      geom_col() +
      coord_flip()
  })
  
  # 5. Correlation heatmap
  output$corPlot <- renderPlot({
    num_data <- filtered() %>%
      select(danceability, energy, tempo, valence, acousticness)
    
    corr <- cor(num_data, use = "complete.obs")
    melt <- reshape2::melt(corr)
    
    ggplot(melt, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2()
  })
  
  # 6. Data table
  output$table <- renderDT({
    datatable(filtered(), selection = "single")
  })
  
  # SELECT → update details
  selected_song <- reactive({
    req(input$table_rows_selected)
    filtered()[input$table_rows_selected, ]
  })
  
  output$songInfo <- renderPrint({
    selected_song()
  })
  
  output$songRadar <- renderPlot({
    req(selected_song())
    s <- selected_song()
    
    values <- as.numeric(s[1, c("danceability","energy","valence","acousticness")])
    barplot(values, names.arg = c("Dance","Energy","Valence","Acoustic"))
  })
}

shinyApp(ui, server)