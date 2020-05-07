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
      tableOutput("table"),
      verbatimTextOutput("stats")
    )
  )
)

# Define server
server <- function(input, output) {
  # Run button pushed
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
    scoringTable <<- calcRobustness(input$nodeTableFile, input$edgeTableFile) # <<- denotes global variable
    print("Finished analysis.")
    
    # Show table of results and stats
    output$table <- renderTable(scoringTable, rownames = FALSE)
    output$stats <- renderPrint(summary(scoringTable$Impact))
  })
  
  # Download button pushed
  output$download <- downloadHandler(
    filename = paste("robustnessAnalysis-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      print("Starting download...")
      write.csv(scoringTable, file, row.names = FALSE)
      print("Finished download.")
    })
}

shinyApp(ui = ui, server = server)