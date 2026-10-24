%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
kmeansExampleFigureVisible = 'off';
run([modulepath('statistics', 'root'), '/examples/kmeans_clustering.m']);

sortedCenters = sortrows(clusterCenters, 1);
centerErrors = abs(sortedCenters - sourceCenters);
assert(isequal(size(clusterData), [75 2]));
assert(isequal(unique(clusterIndex), [1; 2; 3]));
assert(max(centerErrors(:)) < 0.1);
assert(all(withinClusterSums >= 0));
assert(isgraphics(figureHandle));
close(figureHandle);
%=============================================================================
