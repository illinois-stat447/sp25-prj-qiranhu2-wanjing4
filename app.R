library(DBI)        
library(RSQLite)   
library(shiny)     
library(DT)        
library(dplyr)
library(httr)
library(jsonlite)

# --------Just an example to scrape covid data, data might be changed------
# Fetch data from API
url <- "https://disease.sh/v3/covid-19/historical/USA?lastdays=all"
response <- GET(url)

# Convert response to JSON
data <- content(response, as = "text", encoding = "UTF-8")
parsed_data <- fromJSON(data)
# Create a connection to an SQLite database
con <- dbConnect(RSQLite::SQLite(), "covid_data.db")

df <- data.frame(
  country = parsed_data$country,
  cases = unlist(parsed_data$timeline$cases),
  deaths = unlist(parsed_data$timeline$deaths),
  recovered = unlist(parsed_data$timeline$recovered),
  date = as.Date(names(parsed_data$timeline$cases), format = "%m/%d/%y")  # date is parsed wrong, need to fix
)

# Store in SQL database
dbWriteTable(con, "covid_cases", df, overwrite = TRUE)

# Check the stored data
dbListTables(con)

# Fetch records
query <- "SELECT * FROM covid_cases WHERE cases > 10000 ORDER BY date DESC"
result <- dbGetQuery(con, query)

# Display the result
#print(result) # uncomment to see the results

# -------------------------------------------------------------

ui = fluidPage(
  titlePanel("Database Table Viewer with Property Type Search"),
  sidebarLayout(
    sidebarPanel(
      textInput("property_type_search", "Search by Property Type:", ""),
      actionButton("search_btn", "Search")
    ),
    mainPanel(
      DTOutput("table_display")
    )
  )
)

server = function(input, output, session) {
  # Reactive value to store the filtered data
  filtered_data = reactiveVal()
  
  # Update table when search button is clicked
  observeEvent(input$search_btn, {
    # Connect to database
    con = dbConnect(SQLite(), "my_database.db")
    
    # SQL query with property type filter
    if (input$property_type_search == "") {
      # If search is empty, show all data
      query = "SELECT * FROM my_table"
    } else {
      # Filter by property type (using LIKE for partial matches)
      query = sprintf("SELECT * FROM my_table WHERE property_type LIKE '%%%s%%'", 
                       input$property_type_search)
    }
    
    # Execute query and store results
    table_data = dbGetQuery(con, query)
    filtered_data(table_data)
  })
  
  # Show all data
  observe({
    con = dbConnect(SQLite(), "my_database.db")
    table_data = dbGetQuery(con, "SELECT * FROM my_table")
    filtered_data(table_data)
    dbDisconnect(con)
  })
  
  # Render the table
  output$table_display = renderDT({
    datatable(filtered_data())
  })
}

dbDisconnect(con)

shinyApp(ui = ui, server = server)