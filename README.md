# NodeAnalyzer
R Shiny app to calculate the overall impact of each gene in a given gene regulatory network. Also available online at https://rspurney.shinyapps.io/nodeanalyzer/.

## Required packages
[shiny](https://shiny.rstudio.com): `install.packages("shiny")`

## Instructions
1. To calculate necessary network statistics such as outdegree and indegree in Cytoscape, select Tools -> Analyze Network, check the Analyze as Directed Graph if applicable, and then press OK to perform the analysis.
2. To export node and edge files from Cytoscape, select File -> Export -> Table to File... and then choose default edge or default node in the 'Select a table to export' dropdown. Press OK to export each file.
3. Import the node and edge table files into the corresponding prompts above and then press the Run Analysis button to calculate impact scores. Results can be downloaded as a table using the Download Results button.

## Implementation
Each node from the network receives a weight between 1 and 2:

weight(N) = w = 1 + O/O<sub>max</sub>

Nodes with a high outdegree (O) are considered to be more impactful within the network and will thus receive a high weight. The impact of a node within the network topology is calculated based on the weighted first neighbors:

R = ASPL x w<sub>i</sub> + A x w<sub>o</sub>

A = Nodes(outdegree>0)/Nodes

where R = Robustness, ASPL = Average Shortest Path Length, O = outdegree, and I = indegree. A scale-free network will have a low A, while a scale-rich network will have a high A, allowing for the indegree to contribute more to the impact of a node. Because the first neighbors are weighted in regards to their outdegree, genes with a lower outdegree can still have a large impact if its neighbors have a high outdegree and the gene is thus centrally located. Genes with a large number of cascading targets that are 2 or more nodes away will have a higher ASPL and thus a higher scaled outdegree weight, accurately reflecting the hierarchical importance of the source gene itself and its first neighbors targets.

## Citation
GIF1 paper
