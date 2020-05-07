library(shiny)
#library(rsconnect)

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("Robustness Analyzer"),
  # Sidebar layout
  sidebarLayout(
    # Side panel
    sidebarPanel(
      # Node table file selector
      fileInput("nodeTableFile", "Choose Node Table File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      # Edge table file selector
      fileInput("edgeTableFile", "Choose Edge Table File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      # Run button
      actionButton("run", "Run Analysis"),
      # Download button
      downloadButton("download", "Download Results")
      ),
    # Main panel
    mainPanel(
      tableOutput("scoringTable")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$run, {
    # Check for valid node and edge table files
    errorState = FALSE
    if (is.null(input$nodeTableFile)) {
      nodeError <- showNotification("Invalid node table", duration = 5,
                                    closeButton = TRUE, type = "error")
      errorState = TRUE
    }
    if (is.null(input$edgeTableFile)) {
      edgeError <- showNotification("Invalid edge table", duration = 5,
                       closeButton = TRUE, type = "error")
      errorState = TRUE
    }
    if (errorState) {
      return(NULL)
    }
    
    # Perform analysis
    print("Starting analysis...")
    scoringTable <- calcRobustness(input$nodeTableFile, input$edgeTableFile)
    print("Finished analysis.")
    
    # Show table of results
    output$scoringTable <- renderTable(scoringTable, rownames = TRUE)
  })
  
  output$download <- downloadHandler(
    filename = "robustnessAnalysis.csv",
    content = function(file) {
      write.csv(scoringTable, file, row.names = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)