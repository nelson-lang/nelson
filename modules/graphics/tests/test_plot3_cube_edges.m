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
f = figure('Visible', 'off', 'Position', [100 100 640 480], 'Color', [1 1 1]);
vertices = [-1 -1 -1; -1 -1 1; -1 1 -1; -1 1 1; 1 -1 -1; 1 -1 1; 1 1 -1; 1 1 1];
edges = [1 2; 1 3; 1 5; 2 4; 2 6; 3 4; 3 7; 4 8; 5 6; 5 7; 6 8; 7 8];
ax = axes('Parent', f);
hold(ax, 'on');
for k = 1:size(edges, 1)
  plot3(ax, vertices(edges(k, :), 1), vertices(edges(k, :), 2), ...
    vertices(edges(k, :), 3), 'k', 'LineWidth', 2);
end
axis(ax, 'equal');
axis(ax, 'off');
view(ax, 0, 30);
drawnow();
frame = getframe(f);
img = frame.cdata;
mask = img(:, :, 1) < 80 & img(:, :, 2) < 80 & img(:, :, 3) < 80;
asserts.istrue(nnz(mask) > 50);
[rows, cols] = find(mask);
width = max(cols) - min(cols) + 1;
height = max(rows) - min(rows) + 1;
ratio = width / height;
asserts.istrue(ratio > 0.65 && ratio < 0.82);
close(f);
%=============================================================================
