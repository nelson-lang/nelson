%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 360 260], 'Color', [1 1 1]);
vertices = [-1 -1 -1; -1 -1 1; -1 1 -1; -1 1 1; 1 -1 -1; 1 -1 1; 1 1 -1; 1 1 1];
edges = [1 2; 1 3; 1 5; 2 4; 2 6; 3 4; 3 7; 4 8; 5 6; 5 7; 6 8; 7 8];
axes('Parent', f);
axis equal;
axis off;
view(3);
hold on;
edgeCount = size(edges, 1);
edgeX = [vertices(edges(:, 1), 1), vertices(edges(:, 2), 1), nan(edgeCount, 1)]';
edgeY = [vertices(edges(:, 1), 2), vertices(edges(:, 2), 2), nan(edgeCount, 1)]';
edgeZ = [vertices(edges(:, 1), 3), vertices(edges(:, 2), 3), nan(edgeCount, 1)]';
plot3(edgeX(:), edgeY(:), edgeZ(:), 'k', 'LineWidth', 1);
segmentCount = 48;
t = linspace(-1, 1, segmentCount)';
lineX = [-ones(segmentCount, 1), ones(segmentCount, 1), nan(segmentCount, 1)]';
lineY = [t, -t, nan(segmentCount, 1)]';
lineZ = [0.5 * sin((1:segmentCount)' / 3), 0.5 * cos((1:segmentCount)' / 4), ...
  nan(segmentCount, 1)]';
plot3(lineX(:), lineY(:), lineZ(:), 'Color', [0 0.35 0.8], 'LineWidth', 0.75);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
