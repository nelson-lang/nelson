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
close all;
%=============================================================================
f = figure('Visible', 'on', 'Position', [100 100 640 520], 'Color', [1 1 1]);
ax = axes('Parent', f);
fill([0 1 1 0], [0 0 1 1], 'r', 'LineWidth', 12);
hold on;
fill([0.5 1.5 1.5 0.5], [0.5 0.5 1.5 1.5], 'g', 'LineWidth', 1);
axis(ax, [0 1.5 0 1.5]);
drawnow();

frame = getframe(ax);
img = frame.cdata;
render_assert_not_blank(img);

xLimits = ax.XLim;
yLimits = ax.YLim;
height = size(img, 1);
width = size(img, 2);
col = round(1 + (0.75 - xLimits(1)) / (xLimits(2) - xLimits(1)) * (width - 1));
row = round(height - (1.0 - yLimits(1)) / (yLimits(2) - yLimits(1)) * (height - 1));
rows = max(1, row - 4):min(height, row + 4);
cols = max(1, col - 12):min(width, col + 12);
sample = img(rows, cols, :);

greenPixels = sample(:, :, 2) > 180 & sample(:, :, 1) < 90 & sample(:, :, 3) < 90;
darkPixels = sample(:, :, 1) < 80 & sample(:, :, 2) < 80 & sample(:, :, 3) < 80;
asserts.istrue(nnz(greenPixels) / numel(greenPixels) > 0.70);
asserts.istrue(nnz(darkPixels) / numel(darkPixels) < 0.05);

close(f);
close all;
%=============================================================================
