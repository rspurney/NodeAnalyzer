library(shiny)

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
      p("First, to calculate necessary network statistics such as outdegree and indegree in 
        Cytoscape, select Tools -> Analyze Network, check the Analyze as Directed Graph 
        if applicable, and then press OK to perform the analysis."),
      p("To export node and edge files from Cytoscape, select File -> Export -> Table
        to File... and then choose default edge or default node in the 'Select a table
        to export' dropdown. Press OK to export each file."),
      p("Import the node and edge table files into the corresponding prompts above and 
        then press the Run Analysis button to calculate impact scores. Results can be 
        downloaded as a table using the Download Results button.")
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
    
    # Show output table and plots
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
  
  # Function for calculating impact factors
  calcRobustness <- function(nodeTableFile, edgeTableFile) {
    # Import node and edge tables
    nodeTable <- read.csv(nodeTableFile$datapath, header = TRUE)
    nodeDim <- dim(nodeTable)
    row.names(nodeTable) <- nodeTable$shared.name
    edgeTable <- read.csv(edgeTableFile$datapath, header = TRUE)
    edgeDim <- dim(edgeTable)
    
    # Create empty scoring table
    scoringTable <- as.data.frame(matrix(0, ncol = 4, nrow = nodeDim[1]))
    names(scoringTable) <- c("Impact", "Weight", "Outdegree", "Indegree")
    row.names(scoringTable) <- row.names(nodeTable)
    scoringTable[ , "Outdegree"] <- nodeTable[ , "Outdegree"]
    scoringTable[ , "Indegree"] <- nodeTable[ , "Indegree"]
    
    # Calculate weights (1 + outdegree / maxOutdegree)
    maxOutdegree = max(nodeTable$Outdegree)
    for (gene in row.names(nodeTable)) {
      scoringTable[gene, "Weight"] = 1 + (nodeTable[gene, "Outdegree"] / maxOutdegree)
    }
    
    # Calculate indegree factor (# of genes with outdegree > 0)
    indegreeFactor <- 0
    for (gene in row.names(nodeTable)) {
      if (nodeTable[gene, "Outdegree"] > 0) {
        indegreeFactor <- indegreeFactor + 1
      }
    }
    indegreeFactor <- indegreeFactor / nodeDim[1]
    
    # Parse interactions
    interactionTable <- as.data.frame(matrix(0, ncol = 3, nrow = edgeDim[1]))
    names(interactionTable) <- c("Source", "Type", "Target")
    for (interaction in row.names(edgeTable)) {
      interactionTable[interaction , ] <- trimws(t(unlist(strsplit(toString(edgeTable[interaction,
                                                                                      "shared.name"]),
                                                                   "[()]"))))
    }
    
    # Calculate impact factors
    for (gene in row.names(nodeTable)) {
      # Calculate sum of indegree and outdegree weights
      indegreeWeight <- 0
      outdegreeWeight <- 0
      for (interaction in row.names(edgeTable)) {
        # Indegree weights
        if (gene == interactionTable[interaction, "Target"]) {
          indegreeWeight <- indegreeWeight + scoringTable[interactionTable[interaction, "Source"], "Weight"]
        }
        # Outdegree weights
        if (gene == interactionTable[interaction, "Source"]) {
          outdegreeWeight <- outdegreeWeight + scoringTable[interactionTable[interaction, "Target"], "Weight"]
        }
      }
      # Get outdegree factor (average shortest path length for given gene)
      outdegreeFactor <- nodeTable[gene, "AverageShortestPathLength"]
      # Calculate impact factor
      scoringTable[gene, "Impact"] <- (outdegreeFactor * outdegreeWeight) + (indegreeFactor * indegreeWeight)
    }
    
    # Sort by impact factor
    scoringTable <- scoringTable[order(scoringTable$Impact, decreasing = TRUE), ]
    
    # Move row names to first column
    scoringTableOut <- scoringTable
    row.names(scoringTableOut) <- NULL
    scoringTableOut <- cbind(row.names(scoringTable), scoringTableOut)
    names(scoringTableOut)[1] <- "Gene"
    
    return(scoringTableOut)
  }
}

shinyApp(ui = ui, server = server)