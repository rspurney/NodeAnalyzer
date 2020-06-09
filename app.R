library(shiny)
#library(rsconnect)

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("Node Analyzer"),
  
  sidebarLayout(
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
      downloadButton("download", "Download Results"),
      
      # Instructions
      headerPanel(""),
      h4("Instructions"),
      p("Use the app"),
      
      ),
    
    mainPanel(
      tabsetPanel(
        # Score table
        tabPanel("Genes Table", tableOutput("table")),
      
        # Plots
        tabPanel("Impact Plot", plotOutput("impactPlot")),
        tabPanel("Outdegree Plot", plotOutput("outdegreePlot")),
        tabPanel("Indegree Plot", plotOutput("indegreePlot"))
      )
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
    
    # Show outputs
    output$table <- renderTable(scoringTable, rownames = FALSE)
    output$impactPlot <- renderPlot({
      plot(scoringTable$Gene, scoringTable$Impact, xlab = "", ylab = "Impact", cex.axis = 0.75, las = 2)
      })
    output$outdegreePlot <- renderPlot({
      hist(scoringTable$Outdegree, main = "", xlab = "Outdegree", xlim = c(0, max(scoringTable$Outdegree)))
      })
    output$indegreePlot <- renderPlot({
      hist(scoringTable$Indegree, main = "", xlab = "Indegree", xlim = c(0, max(scoringTable$Indegree)))
      })
  })
  
  # Download button pushed
  output$download <- downloadHandler(
    filename = paste("nodeAnalysis-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      print("Starting download...")
      write.csv(scoringTable, file, row.names = FALSE)
      print("Finished download.")
    })
}

shinyApp(ui = ui, server = server)