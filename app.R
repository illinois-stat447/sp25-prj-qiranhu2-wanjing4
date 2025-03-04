library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)

# Connect to SQLite database
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
    all_anime_df <- data.frame() # Initialize an empty data frame to accumulate data
    
    for (page in 1:pages) {
      tryCatch({
        url <- paste0("https://api.jikan.moe/v4/anime?page=", page)
        response <- GET(url)
        
        if (status_code(response) == 200) {
          data <- content(response, "text", encoding = "UTF-8")
          anime_data_json <- fromJSON(data)$data
          
          if (!is.null(anime_data_json) && nrow(anime_data_json) > 0) {
            # Extract genres safely
            genres_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              genres_list <- anime_data_json$genres[[i]]
              if (is.null(genres_list) || length(genres_list) == 0) {
                return("")
              } else {
                return(paste(genres_list$name, collapse = ", "))
              }
            })
            
            # Create data frame with all fields, directly assigning columns
            anime_df <- data.frame(
              mal_id = anime_data_json$mal_id,
              title = anime_data_json$title,
              type = replace(anime_data_json$type, is.na(anime_data_json$type), NA),
              episodes = replace(anime_data_json$episodes, is.na(anime_data_json$episodes), NA),
              status = replace(anime_data_json$status, is.na(anime_data_json$status), NA),
              rating = replace(anime_data_json$rating, is.na(anime_data_json$rating), NA),
              score = replace(anime_data_json$score, is.na(anime_data_json$score), NA),
              scored_by = replace(anime_data_json$scored_by, is.na(anime_data_json$scored_by), NA),
              rank = replace(anime_data_json$rank, is.na(anime_data_json$rank), NA),
              popularity = replace(anime_data_json$popularity, is.na(anime_data_json$popularity), NA),
              members = replace(anime_data_json$members, is.na(anime_data_json$members), NA),
              favorites = replace(anime_data_json$favorites, is.na(anime_data_json$favorites), NA),
              genres = genres_vec
            )
            
            # Accumulate data across pages
            all_anime_df <- rbind(all_anime_df, anime_df)
          }
        } else {
          print(paste("HTTP status code:", status_code(response)))
        }
      }, error = function(e) {
        print(paste("Error fetching page", page, ":", e$message))
      })
      
      Sys.sleep(1)
      incProgress(1 / pages)
    }
    
    # After fetching all pages, drop and rewrite the table
    if (nrow(all_anime_df) > 0) {
      if (dbExistsTable(conn, "anime")) {
        dbExecute(conn, "DROP TABLE anime")
      }
      dbWriteTable(conn, "anime", all_anime_df, overwrite = TRUE)
    }
  })
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Anime Data Fetcher"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pages", "Number of pages to fetch:", value = 5, min = 1),
      actionButton("fetch_show", "Fetch & Show Data"),
      textInput("search", "Search by title:", placeholder = "Enter title keywords")
    ),
    mainPanel(
      DT::dataTableOutput("animeTable")
    )
  )
)

server <- function(input, output, session) {
  data_version <- reactiveVal(0)
  
  observeEvent(input$fetch_show, {
    shinyjs::disable("fetch_show")
    fetch_data(input$pages)
    data_version(data_version() + 1)
    shinyjs::enable("fetch_show")
  })
  
  filtered_data <- reactive({
    data_version()
    if (dbExistsTable(conn, "anime")) {
      if (input$search == "" || is.null(input$search)) {
        dbGetQuery(conn, "SELECT * FROM anime")
      } else {
        search_term <- paste0("%", tolower(input$search), "%")
        dbGetQuery(conn, "SELECT * FROM anime WHERE LOWER(title) LIKE ?", 
                   params = list(search_term))
      }
    } else {
      data.frame(
        mal_id = integer(0),
        title = character(0),
        type = character(0),
        episodes = integer(0),
        status = character(0),
        rating = character(0),
        score = numeric(0),
        scored_by = integer(0),
        rank = integer(0),
        popularity = integer(0),
        members = integer(0),
        favorites = integer(0),
        genres = character(0)
      )
    }
  })
  
  output$animeTable <- DT::renderDataTable({
    datatable(filtered_data(), options = list(searching = FALSE))
  })
  
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui = ui, server = server)