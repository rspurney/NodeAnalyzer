calcRobustness <- function(nodeTableFile, edgeTableFile) {
  # Import node and edge tables
  nodeTable <- read.csv(nodeTableFile$datapath, header = TRUE)
  nodeDim <- dim(nodeTable)
  row.names(nodeTable) <- nodeTable$shared.name
  edgeTable <- read.csv(edgeTableFile$datapath, header = TRUE)
  edgeDim <- dim(edgeTable)
  
  # Create empty scoring table
  scoringTable <- as.data.frame(matrix(0, ncol = 2, nrow = nodeDim[1]))
  names(scoringTable) <- c("Weight", "Impact")
  row.names(scoringTable) <- row.names(nodeTable)
  
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
    # Calculate impact factor
    outdegreeFactor <- nodeTable[gene, "AverageShortestPathLength"]
    scoringTable[gene, "Impact"] <- (outdegreeFactor * outdegreeWeight) + (indegreeFactor * indegreeWeight)
  }
  
  # Sort by impact factor
  scoringTable <- scoringTable[order(scoringTable$Impact, decreasing = TRUE), ]
  
  return(scoringTable)
}
