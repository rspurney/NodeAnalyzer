# NodeAnalyzer
R Shiny app to calculate the overall impact of each gene in a given gene regulatory network. Also available online at https://rspurney.shinyapps.io/nodeanalyzer/.

## Required packages
[shiny](https://shiny.rstudio.com): `install.packages("shiny")`

## Instructions
1. To calculate necessary network statistics such as outdegree and indegree in Cytoscape, select Tools -> Analyze Network, check the Analyze as Directed Graph if applicable, and then press OK to perform the analysis.
2. To export node and edge files from Cytoscape, select File -> Export -> Table to File... and then choose default edge or default node in the 'Select a table to export' dropdown. Press OK to export each file.
3. Import the node and edge table files into the corresponding prompts above and then press the Run Analysis button to calculate impact scores. Results can be downloaded as a table using the Download Results button.

## Implementation
formulas

## Citation
GIF1 paper
