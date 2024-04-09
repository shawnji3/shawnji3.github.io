library(shiny)
library(ggplot2)
library(dplyr)

# Read the dataset
music_data <- read.csv("billboard_used.csv")  # Replace "your_dataset.csv" with the actual file path

# List of genres
genres <- c('r&b', 'pop', 'hip pop', 'salsa', 'soul', 'rock', 'country', 'neo')

# Function to replace genre with matching genre from the list
replace_genre <- function(genre_str, genre_list) {
  for (genre in genre_list) {
    if (genre %in% unlist(strsplit(gsub("\\[|\\]|'", "", genre_str), ", "))) {
      return(genre)
    }
  }
  return(NA)
}

# Modify 'Genres' column to have only the matching genre
music_data$Genres <- sapply(music_data$Genres, replace_genre, genre_list = genres)

# Remove rows with NA in 'Genres' column
music_data <- music_data[complete.cases(music_data$Genres), ]

# Define colors for each genre
genre_colors <- c("#FFB6C1", "#87CEEB", "#FFD700", "#32CD32", "#FF4500", "#9400D3", "#FF1493", "#00FF00")

# Define UI
ui <- fluidPage(
  titlePanel("Genre Counts by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Choose a year:",
                  min = min(music_data$Year),
                  max = max(music_data$Year),
                  value = min(music_data$Year),
                  step = 1,
                  sep = "")
    ),
    mainPanel(
      plotOutput("genre_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to filter data based on selected year and count genre occurrences
  genre_counts <- reactive({
    filtered_data <- filter(music_data, Year == input$year)
    counts <- table(filtered_data$Genres)
    df <- data.frame(genre = names(counts), count = as.numeric(counts))
    df <- df[order(df$count, decreasing = TRUE), ]
    return(df)
  })
  
  # Plot bar graph
  output$genre_plot <- renderPlot({
    ggplot(genre_counts(), aes(x = reorder(genre, -count), y = count, fill = genre)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = genre_colors) +
      labs(x = "Genre", y = "Count", title = paste("Genre Counts in", input$year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
            axis.text.y = element_text(size = 10, color = "black"),
            axis.title = element_text(size = 12, color = "black"),
            plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "black")) +
      geom_text(aes(label = count), vjust = -0.3, size = 3, color = "black") +
      scale_fill_viridis_d()
  })
}

# Run the application
shinyApp(ui = ui, server = server)