# Load required libraries
library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)

# Establish database connection in global scope
conn <- dbConnect(SQLite(), "anime_database.sqlite")

# Define the User Interface (UI)
ui <- fluidPage(
  # Enable shinyjs for dynamic UI control
  useShinyjs(),
  
  # Title of the app
  titlePanel("Anime Data Fetcher"),
  
  # Layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # Input for number of pages to fetch
      numericInput("pages", "Number of pages to fetch:", value = 5, min = 1),
      
      # Button to fetch new data
      actionButton("fetch", "Fetch Data"),
      
      # Button to view existing data
      actionButton("view", "View Existing Data")
    ),
    
    mainPanel(
      # Output for the interactive data table
      DT::dataTableOutput("animeTable")
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  # Reactive value to store and update anime data for display
  anime_data <- reactiveVal()
  
  # Function to fetch anime data from the API
  fetch_data <- function(pages) {
    # Show progress bar during fetching
    withProgress(message = "Fetching data", value = 0, {
      for (page in 1:pages) {
        # Handle potential errors gracefully
        tryCatch({
          # Construct API URL
          url <- paste0("https://api.jikan.moe/v4/anime?page=", page)
          response <- GET(url)
          
          # Check if request was successful
          if (status_code(response) == 200) {
            data <- content(response, "text", encoding = "UTF-8")
            anime_data <- fromJSON(data)$data
            
            # Process data if available
            if (!is.null(anime_data)) {
              # Create data frame from API response
              anime_df <- data.frame(
                mal_id = anime_data$mal_id,
                title = anime_data$title,
                type = anime_data$type,
                episodes = anime_data$episodes,
                status = anime_data$status,
                rating = anime_data$rating,
                score = anime_data$score,
                popularity = anime_data$popularity,
                members = anime_data$members
              )
              
              # Check if table exists; create it if not, otherwise append new data
              if (!dbExistsTable(conn, "anime")) {
                dbWriteTable(conn, "anime", anime_df, overwrite = TRUE)
              } else {
                # Get existing IDs to avoid duplicates
                existing_ids <- dbGetQuery(conn, "SELECT mal_id FROM anime")$mal_id
                new_anime_df <- anime_df[!anime_df$mal_id %in% existing_ids, ]
                if (nrow(new_anime_df) > 0) {
                  dbWriteTable(conn, "anime", new_anime_df, append = TRUE)
                }
              }
            }
          } else {
            showNotification(paste("Failed to fetch page", page), type = "error")
          }
        }, error = function(e) {
          # Notify user of any errors
          showNotification(paste("Error fetching page", page, ":", e$message), type = "error")
        })
        
        # Delay to respect API rate limits (60 requests/min)
        Sys.sleep(1)
        
        # Update progress bar
        incProgress(1 / pages)
      }
      
      # After fetching, update the reactive value with all data from database
      current_data <- dbGetQuery(conn, "SELECT * FROM anime")
      anime_data(current_data)
    })
  }
  
  # Event handler for Fetch Data button
  observeEvent(input$fetch, {
    # Disable button during fetch to prevent multiple clicks
    shinyjs::disable("fetch")
    fetch_data(input$pages)
    shinyjs::enable("fetch")
  })
  
  # Event handler for View Existing Data button
  observeEvent(input$view, {
    # Retrieve and display current database contents
    current_data <- dbGetQuery(conn, "SELECT * FROM anime")
    anime_data(current_data)
  })
  
  # Render the interactive data table
  output$animeTable <- DT::renderDataTable({
    anime_data()
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)