function [genes, nodeTableR] = calcRobustness(inferenceTechnique, nodeTablePath, edgeTablePath)
%% Load tables
nodeTable = readtable(nodeTablePath, 'PreserveVariableNames', true);
[nodeTableR, nodeTableC] = size(nodeTable);

edgeTable = readtable(edgeTablePath, 'PreserveVariableNames', true);
[edgeTableR, edgeTableC] = size(edgeTable);

%% Create scoring table
emptyCol = zeros(nodeTableR, 1);
genes = table(emptyCol, emptyCol);
genes.Properties.VariableNames = {'Weight', 'Impact Factor'};
genes.Properties.RowNames = nodeTable.('shared name');

%% Calculate weights
maxOutdegree = max(nodeTable.('Outdegree'));
for i = 1:nodeTableR
    genes{i, 'Weight'} = 1 + (nodeTable{i, 'Outdegree'} / maxOutdegree);
end

%% Calculate indegree factor
indegreeFactor = 0;
for i = 1:nodeTableR
    if (nodeTable{i, 'Outdegree'}) > 0
        indegreeFactor = indegreeFactor + 1;
    end
end
indegreeFactor = indegreeFactor / nodeTableR;

%% Parse interactions
interactions = table();
for i = 1:edgeTableR
    interactions{:, i} = split(edgeTable{i, 'shared name'});
end

%% Calculate impact factors
for i = 1:nodeTableR
    % Calculate sum of outdegree weights
    outdegreeWeight = 0;
    for j = 1:edgeTableR
        if strcmp(genes.Properties.RowNames{i}, interactions{1, j}{1}) == true
            outdegreeWeight = outdegreeWeight + genes{interactions{3, j}, 1};
        end
    end
    % Calculate sum of indegree weights
    indegreeWeight = 0;
    for j = 1:edgeTableR
        if strcmp(genes.Properties.RowNames{i}, interactions{3, j}{1}) == true
            indegreeWeight = indegreeWeight + genes{interactions{1, j}, 1};
        end
    end
    % Calculate impact factor
    outdegreeFactor = nodeTable{i, 'AverageShortestPathLength'};
    genes{i, 'Impact Factor'} = (outdegreeFactor * outdegreeWeight) + (indegreeFactor * indegreeWeight);
end

genes = sortrows(genes, 'Impact Factor', 'descend');
end