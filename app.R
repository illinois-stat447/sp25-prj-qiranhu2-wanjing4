library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)

conn = dbConnect(SQLite(), "anime_database.sqlite")

fetch_genres = function() {
  genre_url = "https://api.jikan.moe/v4/genres/anime"
  response = GET(genre_url)
  
  if (status_code(response) == 200) {
    genre_data = fromJSON(content(response, "text", encoding = "UTF-8"))$data
    genre_df = data.frame(
      genre_id = genre_data$mal_id,
      genre_name = genre_data$name
    )
    return(genre_df)
  } else {
    print("Failed to fetch genres")
    return(NULL)
  }
}

fetch_data = function(pages) {
  withProgress(message = "Fetching anime data", value = 0, {
    genres_df = fetch_genres()
    
    for (page in 1:pages) {
      tryCatch({
        url = paste0("https://api.jikan.moe/v4/anime?page=", page)
        response = GET(url)
        
        if (status_code(response) == 200) {
          data = content(response, "text", encoding = "UTF-8")
          anime_data_json = fromJSON(data)$data
          
          if (!is.null(anime_data_json)) {
            anime_df = data.frame(
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
              existing_ids = dbGetQuery(conn, "SELECT mal_id FROM anime")$mal_id
              new_anime_df = anime_df[!anime_df$mal_id %in% existing_ids, ]
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

ui = fluidPage(
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

# Server with SQL-based search filtering
server = function(input, output, session) {
  # Reactive value to trigger updates when new data is fetched
  data_version = reactiveVal(0)
  
  # Event handler for fetching data
  observeEvent(input$fetch_show, {
    shinyjs::disable("fetch_show")
    fetch_data(input$pages)
    data_version(data_version() + 1) # Increment to trigger table update
    shinyjs::enable("fetch_show")
  })
  
  # Reactive expression to filter data based on search input
  filtered_data = reactive({
    data_version() # Trigger update when data_version changes
    if (dbExistsTable(conn, "anime")) {
      if (input$search == "" || is.null(input$search)) {
        # Return all data if search input is empty
        dbGetQuery(conn, "SELECT * FROM anime")
      } else {
        # Filter data based on search input (case-insensitive)
        search_term = paste0("%", tolower(input$search), "%")
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
        popularity = integer(0),
        members = integer(0),
        genres = character(0)
      )
    }
  })

  output$animeTable = DT::renderDataTable({
    datatable(filtered_data(), options = list(searching = FALSE))
  })
}

shinyApp(ui = ui, server = server)