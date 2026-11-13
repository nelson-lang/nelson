%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Partition three overlapping Gaussian clouds with k-means (k-means++ seeding).
rng(3);
pointsPerCluster = 80;
sourceCenters = [-2.0 -0.8; 0.3 1.7; 2.1 -0.4];
sourceSpread = [0.55 0.45 0.60];
clusterData = zeros(3 * pointsPerCluster, 2);
sourceLabel = zeros(3 * pointsPerCluster, 1);
for sourceNumber = 1:3
  rows = (sourceNumber - 1) * pointsPerCluster + (1:pointsPerCluster);
  clusterData(rows, :) = sourceCenters(sourceNumber, :) + ...
    sourceSpread(sourceNumber) * randn(pointsPerCluster, 2);
  sourceLabel(rows) = sourceNumber;
end
[clusterIndex, clusterCenters, withinClusterSums] = kmeans(clusterData, 3, 'Replicates', 5);
% Match each found cluster to its nearest source centre to score the partition.
[~, nearestSource] = min(sum((permute(clusterCenters, [1 3 2]) - ...
  permute(sourceCenters, [3 1 2])) .^ 2, 3), [], 2);
agreement = mean(nearestSource(clusterIndex) == sourceLabel);

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
    18, clusterColors(clusterNumber, :), 'filled');
end
scatter(axesHandle, clusterCenters(:, 1), clusterCenters(:, 2), ...
  120, 'k', 'x', 'LineWidth', 2);
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
grid(axesHandle, 'on');
xlabel(axesHandle, 'Feature 1');
ylabel(axesHandle, 'Feature 2');
dataMargin = 0.4;
xlim(axesHandle, [min(clusterData(:, 1)) - dataMargin, max(clusterData(:, 1)) + dataMargin]);
ylim(axesHandle, [min(clusterData(:, 2)) - dataMargin, max(clusterData(:, 2)) + dataMargin]);
title(axesHandle, sprintf('K-means: %.1f%% of points in their source cluster, within-cluster SSE %.1f', ...
  100 * agreement, sum(withinClusterSums)));
%=============================================================================
