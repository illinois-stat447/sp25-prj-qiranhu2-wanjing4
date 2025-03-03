library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)

conn <- dbConnect(SQLite(), "anime_database.sqlite")

fetch_genres <- function() {
  genre_url <- "https://api.jikan.moe/v4/genres/anime"
  response <- GET(genre_url)
  
  if (status_code(response) == 200) {
    genre_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
    genre_df <- data.frame(
      genre_id = genre_data$mal_id,
      genre_name = genre_data$name
    )
    return(genre_df)
  } else {
    print("Failed to fetch genres")
    return(NULL)
  }
}

fetch_data <- function(pages) {
  withProgress(message = "Fetching anime data", value = 0, {
    genres_df <- fetch_genres()
    
    for (page in 1:pages) {
      tryCatch({
        url <- paste0("https://api.jikan.moe/v4/anime?page=", page)
        response <- GET(url)
        
        if (status_code(response) == 200) {
          data <- content(response, "text", encoding = "UTF-8")
          anime_data_json <- fromJSON(data)$data
          
          if (!is.null(anime_data_json)) {
            anime_df <- data.frame(
              mal_id = anime_data_json$mal_id,
              title = anime_data_json$title,
              type = anime_data_json$type,
              episodes = anime_data_json$episodes,
              status = anime_data_json$status,
              rating = anime_data_json$rating,
              score = anime_data_json$score,
              popularity = anime_data_json$popularity,
              members = anime_data_json$members,
              genres = sapply(anime_data_json$genres, function(g) paste(g$name, collapse = ", "))
            )
            
            if (!dbExistsTable(conn, "anime")) {
              dbWriteTable(conn, "anime", anime_df, overwrite = TRUE)
            } else {
              existing_ids <- dbGetQuery(conn, "SELECT mal_id FROM anime")$mal_id
              new_anime_df <- anime_df[!anime_df$mal_id %in% existing_ids, ]
              if (nrow(new_anime_df) > 0) {
                dbWriteTable(conn, "anime", new_anime_df, append = TRUE)
              }
            }
          }
        }
      }, error = function(e) {
        print(paste("Error fetching page", page, ":", e$message))
      })
      
      Sys.sleep(1)
      incProgress(1 / pages)
    }
  })
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Anime Data Fetcher"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pages", "Number of pages to fetch:", value = 5, min = 1),
      actionButton("fetch_show", "Fetch & Show Data")
    ),
    mainPanel(
      DT::dataTableOutput("animeTable")
    )
  )
)

server <- function(input, output, session) {
  anime_data <- reactiveVal()
  
  observeEvent(input$fetch_show, {
    shinyjs::disable("fetch_show")
    
    # Fetch new data
    fetch_data(input$pages)
    
    # Load all data from the database and display it
    current_data <- dbGetQuery(conn, "SELECT * FROM anime")
    anime_data(current_data)
    
    shinyjs::enable("fetch_show")
  })
  
  output$animeTable <- DT::renderDataTable({
    anime_data()
  })
}

shinyApp(ui = ui, server = server)