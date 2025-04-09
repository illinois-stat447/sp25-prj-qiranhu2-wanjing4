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
library(shinythemes)
library(tidyr)
library(xgboost)

conn <- dbConnect(SQLite(), "anime_database.sqlite")

fetch_genres <- function() {
  genre_url <- "https://api.jikan.moe/v4/genres/anime"
  response <- GET(genre_url)
  if (status_code(response) == 200) {
    genre_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
    genre_df <- data.frame(
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

fetch_data <- function(pages) {
  withProgress(message = "Fetching latest anime data", value = 0, {
    init_resp <- GET("https://api.jikan.moe/v4/anime")
    if (status_code(init_resp) != 200) {
      print("Failed to retrieve pagination info")
      return(NULL)
    }
    init_data <- fromJSON(content(init_resp, "text", encoding = "UTF-8"))
    last_page <- init_data$pagination$last_visible_page
    start_page <- last_page
    end_page <- max(1, last_page - pages + 1)
    genres_df <- fetch_genres()
    all_anime_df <- data.frame()
    for (page in start_page:end_page) {
      tryCatch({
        url <- paste0("https://api.jikan.moe/v4/anime?page=", page)
        response <- GET(url)
        if (status_code(response) == 200) {
          data <- content(response, "text", encoding = "UTF-8")
          anime_data_json <- fromJSON(data)$data
          if (!is.null(anime_data_json) && nrow(anime_data_json) > 0) {
            genres_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              genres_list <- anime_data_json$genres[[i]]
              if (is.null(genres_list) || length(genres_list) == 0) {
                return("")
              } else {
                return(paste(genres_list$name, collapse = ", "))
              }
            })
            studios_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              studios_list <- anime_data_json$studios[[i]]
              if (is.null(studios_list) || length(studios_list) == 0) {
                return("")
              } else {
                return(paste(studios_list$name, collapse = ", "))
              }
            })
            producers_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              producers_list <- anime_data_json$producers[[i]]
              if (is.null(producers_list) || length(producers_list) == 0) {
                return("")
              } else {
                return(paste(producers_list$name, collapse = ", "))
              }
            })
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
              genres = genres_vec,
              studios = studios_vec,
              producers = producers_vec,
              stringsAsFactors = FALSE
            )
            all_anime_df <- rbind(all_anime_df, anime_df)
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

ui <- fluidPage(
  fluidPage(theme = shinytheme("united")),
  useShinyjs(),
  titlePanel("Anime Data Fetcher"),
  sidebarLayout(
    sidebarPanel(
      numericInput("pages", "Number of pages to fetch:", value = 5, min = 1),
      actionButton("fetch_show", "Fetch & Show Data"),
      div(id = "filter_inputs",
          textInput("search", "Search by title:", placeholder = "Enter title keywords"),
          sliderInput("score", "Score (0 to 10)", min = 0, max = 10, step = 0.1, value = c(0, 10)),
          uiOutput("rating"),
          uiOutput("type"),
          uiOutput("genre_filter")
      ),
      actionButton("reset_filters", "Reset Filters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("animeTable")),
        tabPanel("Interactive Data Visualization", 
                 fluidRow(
                   column(12, plotlyOutput("scatterPlot")),
                   column(12, plotlyOutput("polarBarChart")),
                   column(12, plotlyOutput("areaPlot"))
                 )
        ),
        tabPanel(
          "Recommendations", DT::dataTableOutput("recommendTable")
        )
      )
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
  
  observeEvent(input$reset_filters, {
    reset("filter_inputs")
  })
  
  rating_type <- reactive({
    if (!dbExistsTable(conn, "anime")) return(NULL)
    dbGetQuery(conn, "SELECT DISTINCT rating FROM anime WHERE rating IS NOT NULL")
  })
  
  output$rating <- renderUI({
    selectInput("rating", "Rating (PG, PG13, etc)", 
                choices = rating_type() %>% pull(rating),
                selected = NULL, multiple = TRUE)
  })
  
  type_type <- reactive({
    if (!dbExistsTable(conn, "anime")) return(NULL)
    dbGetQuery(conn, "SELECT DISTINCT type FROM anime WHERE type IS NOT NULL")
  })
  
  output$type <- renderUI({
    selectInput("type", "Type (OVA, TV, etc)", 
                choices = type_type() %>% pull(type),
                selected = NULL, multiple = TRUE)
  })
  
  available_genres <- reactive({
    query <- "
    SELECT DISTINCT trim(json_each.value, '\"') AS genre
    FROM anime,
         json_each('[' || '\"' || replace(genres, ', ', '\",\"') || '\"' || ']')
    WHERE genres <> ''
  "
    res <- dbGetQuery(conn, query)
    if(nrow(res) > 0) res$genre else character(0)
  })
  
  output$genre_filter <- renderUI({
    genres <- available_genres()
    selectInput("genre_filter", "Filter by Genre", choices = genres, 
                selected = NULL, multiple = TRUE)
  })
  
  filtered_data <- reactive({
    data_version()
    if (!dbExistsTable(conn, "anime")) {
      return(data.frame(
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
      ))
    }
    base_query <- "SELECT * FROM anime"
    conditions <- c()
    params <- list()
    if (!is.null(input$search) && input$search != "") {
      conditions <- c(conditions, "LOWER(title) LIKE ?")
      params <- c(params, paste0("%", tolower(input$search), "%"))
    }
    if (!is.null(input$score) && length(input$score) == 2) {
      conditions <- c(conditions, "score BETWEEN ? AND ?")
      params <- c(params, input$score[1], input$score[2])
    }
    if (!is.null(input$rating) && length(input$rating) > 0) {
      placeholder <- paste(rep("?", length(input$rating)), collapse = ", ")
      conditions <- c(conditions, paste0("rating IN (", placeholder, ")"))
      params <- c(params, input$rating)
    }
    if (!is.null(input$type) && length(input$type) > 0) {
      placeholder <- paste(rep("?", length(input$type)), collapse = ", ")
      conditions <- c(conditions, paste0("type IN (", placeholder, ")"))
      params <- c(params, input$type)
    }
    if (length(conditions) > 0) {
      query <- paste(base_query, "WHERE", paste(conditions, collapse = " AND "))
    } else {
      query <- base_query
    }
    dbGetQuery(conn, query, params = params)
  })
  
  output$animeTable <- DT::renderDataTable({
    orig_data <- filtered_data() %>% mutate(row_id = row_number())
    onehot <- orig_data %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(genres_list) %>%
      mutate(dummy = 1) %>%
      pivot_wider(id_cols = row_id, 
                  names_from = genres_list, 
                  values_from = dummy, 
                  values_fill = list(dummy = 0))
    final_data <- left_join(orig_data, onehot, by = "row_id") %>% 
      select(-row_id) %>%
      mutate(
        fav_pct = ifelse(members > 0, favorites / members * 100, NA),
        scored_pct = ifelse(members > 0, scored_by / members * 100, NA)
      )
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      final_data <- final_data %>% filter(if_all(all_of(input$genre_filter), ~ . == 1))
    }
    datatable(final_data, options = list(searching = FALSE))
  })
  
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()
    data <- data[!is.na(data$score) & !is.na(data$members), ]
    if(nrow(data) == 0) return(NULL)
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
      ),
      text = ~paste("Title:", title,
                    "<br>Score:", score,
                    "<br>Viewers:", members),
      hoverinfo = "text"
    ) %>% layout(
      title = "Score vs. Viewers",
      xaxis = list(title = "Score", showgrid = TRUE, zeroline = FALSE),
      yaxis = list(title = "Viewers", showgrid = TRUE, zeroline = FALSE),
      plot_bgcolor = "rgba(240,240,240,0.95)",
      paper_bgcolor = "rgba(240,240,240,0.95)"
    )
  })
  
  output$areaPlot <- renderPlotly({
    data <- filtered_data()
    data <- data %>% filter(!is.na(score), !is.na(members), members > 0)
    if(nrow(data) == 0) return(NULL)
    data <- data %>%
      mutate(
        fav_pct = favorites / members * 100,
        scored_pct = scored_by / members * 100,
        score_bin = round(score, 1)
      )
    area_data <- data %>%
      group_by(score_bin) %>%
      summarize(
        avg_fav_pct = mean(fav_pct, na.rm = TRUE),
        avg_scored_pct = mean(scored_pct, na.rm = TRUE)
      ) %>%
      arrange(score_bin)
    fig <- plot_ly(type = 'scatter', mode = 'none', fill = 'tozeroy')
    fig <- fig %>% add_trace(
      x = ~area_data$score_bin,
      y = ~area_data$avg_fav_pct,
      name = 'Favorite %',
      fillcolor = 'rgba(168, 216, 234, 0.5)'
    )
    fig <- fig %>% add_trace(
      x = ~area_data$score_bin,
      y = ~area_data$avg_scored_pct,
      name = 'Scored %',
      fill = 'tozeroy',
      fillcolor = 'rgba(255, 212, 96, 0.5)'
    )
    fig <- fig %>% layout(
      xaxis = list(title = 'Score'),
      yaxis = list(title = 'Percentage'),
      title = list(text = "Score vs. Percentage Metrics", x = 0.5),
      margin = list(t = 100)
    )
    fig
  })
  
  output$polarBarChart <- renderPlotly({
    data <- filtered_data()
    data <- data[!is.na(data$score) & data$score > 0, ]
    if(nrow(data) == 0) return(NULL)
    expanded <- strsplit(data$genres, ",\\s*")
    df_list <- lapply(seq_along(expanded), function(i) {
      if(length(expanded[[i]]) == 0 || expanded[[i]][1] == "") return(NULL)
      data.frame(
        genre = expanded[[i]],
        score = data$score[i],
        stringsAsFactors = FALSE
      )
    })
    merged_df <- do.call(rbind, df_list)
    if(is.null(merged_df) || nrow(merged_df) == 0) return(NULL)
    summary_df <- merged_df %>%
      group_by(genre) %>%
      summarize(
        freq = n(),
        avg_score = mean(score, na.rm = TRUE)
      ) %>%
      arrange(desc(freq))
    top15_df <- summary_df %>%
      slice_head(n = 15) %>%
      arrange(desc(avg_score))
    plot_ly(type = 'barpolar') %>%
      add_trace(
        r = top15_df$avg_score,
        theta = top15_df$genre,
        text = paste("Genre:", top15_df$genre,
                     "<br>Avg Score:", round(top15_df$avg_score, 2),
                     "<br>Frequency:", top15_df$freq),
        hoverinfo = "text",
        marker = list(
          color = top15_df$avg_score,
          colorscale = "RdPu",
          cmin = min(top15_df$avg_score, na.rm = TRUE),
          cmax = max(top15_df$avg_score, na.rm = TRUE)
        )
      ) %>%
      layout(
        title = list(text = "Top 15 Genres by Average Score (Radial Bar)", x = 0.5),
        margin = list(t = 120),
        polar = list(
          radialaxis = list(
            range = c(0, max(top15_df$avg_score, na.rm = TRUE) * 1.1),
            visible = TRUE, showline = FALSE
          ),
          angularaxis = list(direction = "clockwise")
        ),
        showlegend = FALSE
      )
  })
  
  output$recommendTable <- DT::renderDataTable({
    orig_data <- filtered_data() %>% 
      filter(!is.na(score)) %>%
      mutate(row_id = row_number())
    
    onehot <- orig_data %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(genres_list, keep_empty = TRUE) %>%
      mutate(dummy = 1) %>%
      pivot_wider(id_cols = row_id, 
                  names_from = genres_list, 
                  values_from = dummy, 
                  values_fill = list(dummy = 0))
    
    final_data <- left_join(orig_data, onehot, by = "row_id") %>% 
      select(-row_id) %>%
      mutate(
        fav_pct = ifelse(members > 0, favorites/members*100, NA),
        scored_pct = ifelse(members > 0, scored_by/members*100, NA)
      ) %>%
      distinct(mal_id, .keep_all = TRUE)  
    
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      final_data <- final_data %>% filter(if_all(all_of(input$genre_filter), ~ . == 1))
    }
    
    genre_cols <- setdiff(colnames(onehot), "row_id")
    X <- as.matrix(final_data[, genre_cols])
    y <- final_data$score
    dtrain <- xgb.DMatrix(data = X, label = y)
    params <- list(objective = "reg:squarederror", eval_metric = "rmse", eta = 0.1, max_depth = 6)
    
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 50,
      verbose = 0, 
      gamma = 1
    )
    
    final_data$recommendation_score <- predict(xgb_model, X)
    final_data <- final_data %>% 
      arrange(desc(recommendation_score)) %>%
      head(50)
    datatable(final_data, options = list(searching = FALSE))
  })
  
}

shinyApp(ui = ui, server = server)