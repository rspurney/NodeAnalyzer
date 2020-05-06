library(shiny)
library(here)
#library(rsconnect)

# Find app directory and source robustness function
appDirectory = here()
source(paste(appDirectory, "/calcRobustness.R", sep = ""))

# Define UI
ui <- fluidPage(
  titlePanel("Robustness Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("nodeTableFile", "Choose Node Table File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
  
      fileInput("edgeTableFile", "Choose Edge Table File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
  
      actionButton("run", "Run Analysis"),
  
      downloadButton("download", "Download Results")
      ),
    
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