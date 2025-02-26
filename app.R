library(DBI)        
library(RSQLite)   
library(shiny)     
library(DT)        
library(dplyr)      

# Create database connection and load CSV
con = dbConnect(SQLite(), "my_database.db")
data = read.csv("data/Airbnb_Data.csv") 
dbWriteTable(con, "my_table", data, overwrite = TRUE)

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