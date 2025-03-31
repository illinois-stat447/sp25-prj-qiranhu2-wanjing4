library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)

conn = dbConnect(SQLite(), "anime_database.sqlite")

fetch_genres = function() {
  genre_url = "https://api.jikan.moe/v4/genres/anime"
  response = GET(genre_url)
  
  if (status_code(response) == 200) {
    genre_data = fromJSON(content(response, "text", encoding = "UTF-8"))$data
    genre_df = data.frame(
      genre_id = genre_data$mal_id,
      genre_name = genre_data$name,
      stringsAsFactors = FALSE
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
    all_anime_df = data.frame() 
    
    for (page in 1:pages) {
      tryCatch({
        url = paste0("https://api.jikan.moe/v4/anime?page=", page)
        response = GET(url)
        
        if (status_code(response) == 200) {
          data = content(response, "text", encoding = "UTF-8")
          anime_data_json = fromJSON(data)$data
          
          if (!is.null(anime_data_json) && nrow(anime_data_json) > 0) {
            genres_vec = sapply(seq_len(nrow(anime_data_json)), function(i) {
              genres_list = anime_data_json$genres[[i]]
              if (is.null(genres_list) || length(genres_list) == 0) {
                return("")
              } else {
                return(paste(genres_list$name, collapse = ", "))
              }
            })
            
            studios_vec = sapply(seq_len(nrow(anime_data_json)), function(i) {
              studios_list = anime_data_json$studios[[i]]
              if (is.null(studios_list) || length(studios_list) == 0) {
                return("")
              } else {
                return(paste(studios_list$name, collapse = ", "))
              }
            })
            
            producers_vec = sapply(seq_len(nrow(anime_data_json)), function(i) {
              producers_list = anime_data_json$producers[[i]]
              if (is.null(producers_list) || length(producers_list) == 0) {
                return("")
              } else {
                return(paste(producers_list$name, collapse = ", "))
              }
            })
            
            anime_df = data.frame(
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
              genres = genres_vec,
              studios = studios_vec,
              producers = producers_vec,
              stringsAsFactors = FALSE
            )
            
            all_anime_df = rbind(all_anime_df, anime_df)
          }
        } 
      }, error = function(e) {
        print(paste("Error fetching page", page, ":", e$message))
      })
      
      Sys.sleep(1)
      incProgress(1 / pages)
    }
    
    if (nrow(all_anime_df) > 0) {
      if (dbExistsTable(conn, "anime")) {
        dbExecute(conn, "DROP TABLE anime")
      }
      dbWriteTable(conn, "anime", all_anime_df, overwrite = TRUE)
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
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("animeTable")),
        tabPanel("Interactive Data Visualization", 
                 fluidRow(
                   column(12, plotlyOutput("scatterPlot")),
                   column(12, plotlyOutput("bubblePlot"))
                 )
        )
      )
    )
  )
)

server = function(input, output, session) {
  data_version = reactiveVal(0)
  
  observeEvent(input$fetch_show, {
    shinyjs::disable("fetch_show")
    fetch_data(input$pages)
    data_version(data_version() + 1)
    shinyjs::enable("fetch_show")
  })
  
  filtered_data = reactive({
    data_version()
    if (dbExistsTable(conn, "anime")) {
      if (input$search == "" || is.null(input$search)) {
        dbGetQuery(conn, "SELECT * FROM anime")
      } else {
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
        scored_by = integer(0),
        rank = integer(0),
        popularity = integer(0),
        members = integer(0),
        favorites = integer(0),
        genres = character(0),
        studios = character(0),
        producers = character(0),
        stringsAsFactors = FALSE
      )
    }
  })
  
  output$animeTable = DT::renderDataTable({
    datatable(filtered_data(), options = list(searching = FALSE))
  })
  
  output$scatterPlot = renderPlotly({
    data = filtered_data()
    data = data[!is.na(data$score) & !is.na(data$members), ]
    if (nrow(data) == 0) return(NULL)
    
    plot_ly(
      data,
      x = ~score,
      y = ~members,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 10,
        color = ~score,
        colorscale = list(
          c(0, "#F4F7F7"),
          c(0.3, "#D5E5E5"), 
          c(0.5, "#AACFD0"), 
          c(0.7, "#79A8A9"), 
          c(0.9, "#1F4E5F"),
          c(1, "#18230F")
        ),
        cmin = min(data$score, na.rm = TRUE),
        cmax = max(data$score, na.rm = TRUE),
        colorbar = list(title = "Score")
        # Remove line property completely
      ),
      text = ~paste("Title:", title,
                    "<br>Score:", score,
                    "<br>Viewers:", members),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Score vs. Viewers",
        xaxis = list(title = "Score", showgrid = TRUE, zeroline = FALSE),
        yaxis = list(title = "Viewers", showgrid = TRUE, zeroline = FALSE),
        plot_bgcolor = "rgba(240,240,240,0.95)",
        paper_bgcolor = "rgba(240,240,240,0.95)"
      )
  })
  
  output$bubblePlot = renderPlotly({
    data = filtered_data()
    data = data[!is.na(data$favorites) & !is.na(data$score) & !is.na(data$popularity), ]
    if (nrow(data) == 0) return(NULL)
    

    p = plot_ly()
    unique_genres = unique(data$genres)
    genre_colors = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(unique_genres))
    
    for (i in 1:length(unique_genres)) {
      if (unique_genres[i] == "") next
      
      genre_data = data[data$genres == unique_genres[i], ]
      if (nrow(genre_data) > 0) {
        p = add_trace(p,
                       data = genre_data,
                       x = ~popularity,
                       y = ~score,
                       type = 'scatter',
                       mode = 'markers',
                       marker = list(
                         size = ~sqrt(favorites)/10, 
                         sizemode = 'area',
                         opacity = 0.7,
                         color = genre_colors[i],
                         line = list(width = 1, color = 'black')
                       ),
                       text = ~paste("Title:", title,
                                     "<br>Genres:", genres,
                                     "<br>Favorites:", favorites,
                                     "<br>Score:", score,
                                     "<br>Popularity:", popularity),
                       hoverinfo = "text",
                       name = unique_genres[i]
        )
      }
    }
    p %>% layout(
      title = "Favorites vs. Score & Popularity by Genre Combination",
      xaxis = list(title = "Popularity", showgrid = TRUE, zeroline = FALSE),
      yaxis = list(title = "Score", showgrid = TRUE, zeroline = FALSE),
      plot_bgcolor = "rgba(240,240,240,0.95)",
      paper_bgcolor = "rgba(240,240,240,0.95)"
    )
  })
  
  onStop(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui = ui, server = server)
