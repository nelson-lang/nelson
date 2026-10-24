%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
asserts.isequal(nargin('dendrogram'), -1);
asserts.isequal(nargout('dendrogram'), -1);
%=============================================================================
Z = [1 2 1; 3 5 2; 4 6 4];
f = figure('Visible', 'off');
[H, T, outperm] = dendrogram(Z, 0, 'Labels', {'a', 'b', 'c', 'd'});
asserts.isequal(length(H), 3);
asserts.isequal(T, [1; 2; 3; 4]);
asserts.isequal(outperm, [1 2 3 4]);
asserts.isequal(H(1).Type, 'line');
asserts.isequal(H(1).XData, [1 1 2 2]);
asserts.isequal(H(1).YData, [0 1 1 0]);
ax = ancestor(H(1), 'axes');
asserts.isequal(ax.XTick, 1:4);
asserts.isequal(ax.XTickLabel, {'a'; 'b'; 'c'; 'd'});
frame = getframe(f);
assert_not_blank_local(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[H, T, outperm] = dendrogram(Z, 2, 'Reorder', [4 3 1 2], 'Orientation', 'left');
asserts.isequal(length(H), 1);
asserts.isequal(T, [2; 2; 2; 1]);
asserts.isequal(outperm, [1 2]);
ax = ancestor(H(1), 'axes');
asserts.isequal(ax.YTick, 1:2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
[H, T, outperm] = dendrogram(ax, Z, 0, 'Orientation', 'right', ...
  'ColorThreshold', 'default', 'ShowCut', true, 'ShowMarkers', true);
asserts.isequal(length(H), 5);
asserts.isequal(H(1).Parent, ax);
asserts.isequal(T, [1; 2; 3; 4]);
asserts.isequal(outperm, [1 2 3 4]);
asserts.isequal(H(end - 1).Marker, 'o');
asserts.isequal(H(end).LineStyle, '--');
close(f);
%=============================================================================
f = figure('Visible', 'off');
clusterIds = [1; 1; 2; 2];
[H, T] = dendrogram(Z, 3, 'ClusterIndices', clusterIds, 'ShowMarkers', true, ...
  'CheckCrossing', false, 'Orientation', 'bottom');
asserts.isequal(length(T), 4);
asserts.istrue(length(H) >= 3);
close(f);
%=============================================================================
asserts.checkerror('dendrogram()', _('Not enough input arguments.'));
asserts.checkerror('dendrogram([1 2])', _('Tree must be a real numeric matrix with three columns.'));
asserts.checkerror('dendrogram([1 2 -1])', _('Tree must contain finite nonnegative distances.'));
asserts.checkerror('dendrogram([1 4 1])', _('Tree contains invalid node identifiers.'));
asserts.checkerror('dendrogram(Z, -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'P'));
asserts.checkerror('dendrogram(Z, ''Orientation'', ''bad'')', _('Orientation must be ''top'', ''bottom'', ''left'', or ''right''.'));
asserts.checkerror('dendrogram(Z, ''Labels'', {''a'', ''b''})', _('Labels must contain one value per observation.'));
asserts.checkerror('dendrogram(Z, ''Reorder'', [1 2 2 4])', _('Reorder must be a permutation vector.'));
asserts.checkerror('dendrogram(Z, ''ShowCut'', 2)', sprintf(_('%s must be a logical scalar.'), 'ShowCut'));
asserts.checkerror('dendrogram(Z, ''ClusterIndices'', [1 2])', _('ClusterIndices must contain one integer per observation.'));
asserts.checkerror('dendrogram(Z, ''Unknown'', 1)', _('Invalid name-value argument name.'));
%=============================================================================
function assert_not_blank_local(img)
  values = double(img(:));
  asserts.istrue(max(values) > min(values));
end
%=============================================================================
