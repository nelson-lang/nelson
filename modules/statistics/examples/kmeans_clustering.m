%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Partition a deterministic two-dimensional dataset with k-means.
clusterAngles = linspace(0, 2 * pi, 25).';
clusterOffsets = [0.40 * cos(clusterAngles), 0.25 * sin(clusterAngles)];
sourceCenters = [-2.0 -0.8; 0.3 1.7; 2.1 -0.4];
clusterData = [...
  clusterOffsets + sourceCenters(1, :); ...
  clusterOffsets * [0.8 -0.3; 0.3 0.8] + sourceCenters(2, :); ...
  clusterOffsets * [0.6 0.4; -0.4 0.6] + sourceCenters(3, :)];
initialCenters = sourceCenters + [0.15 -0.10; -0.10 0.12; 0.08 0.10];
[clusterIndex, clusterCenters, withinClusterSums] = kmeans(...
  clusterData, 3, 'Start', initialCenters, 'Replicates', 1);

if ~exist('kmeansExampleFigureVisible', 'var')
  kmeansExampleFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'K-means clustering', ...
  'NumberTitle', 'off', ...
  'Visible', kmeansExampleFigureVisible);
axesHandle = axes('Parent', figureHandle);
hold(axesHandle, 'on');
clusterColors = lines(3);
for clusterNumber = 1:3
  members = clusterIndex == clusterNumber;
  scatter(axesHandle, clusterData(members, 1), clusterData(members, 2), ...
    28, clusterColors(clusterNumber, :), 'filled');
end
scatter(axesHandle, clusterCenters(:, 1), clusterCenters(:, 2), ...
  120, 'k', 'x', 'LineWidth', 2);
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
grid(axesHandle, 'on');
xlabel(axesHandle, 'Feature 1');
ylabel(axesHandle, 'Feature 2');
title(axesHandle, sprintf('K-means clustering, total distance %.3f', ...
  sum(withinClusterSums)));
%=============================================================================
