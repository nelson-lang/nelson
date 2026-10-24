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
f = figure('Visible', 'on', 'Position', [100 100 800 600], 'Color', [1 1 1]);
ax1 = axes('Position', [0.1 0.1 0.7 0.7]);
plot(ax1, [0 10], [9 9], 'r', 'LineWidth', 8);
xlim(ax1, [0 10]);
ylim(ax1, [0 10]);

ax2 = axes('Position', [0.65 0.65 0.28 0.28], 'Color', [1 1 1]);
x = linspace(0, 10, 200);
plot(ax2, x, cos(x), 'b', 'LineWidth', 1);
drawnow();

frame = getframe(f);
img = frame.cdata;
render_assert_not_blank(img);

height = size(img, 1);
width = size(img, 2);
ax2PixelPosition = round([0.65 * width, 0.65 * height, 0.28 * width, 0.28 * height]);
x1 = max(1, ax2PixelPosition(1) + 30);
x2 = min(width, ax2PixelPosition(1) + ax2PixelPosition(3) - 30);
yFromBottom = ax2PixelPosition(2) + round(0.70 * ax2PixelPosition(4));
row = height - yFromBottom;
y1 = max(1, row - 8);
y2 = min(height, row + 8);

sample = img(y1:y2, x1:x2, :);
redPixels = sample(:, :, 1) > 180 & sample(:, :, 2) < 90 & sample(:, :, 3) < 90;
asserts.istrue(nnz(redPixels) / numel(redPixels) < 0.01);

close(f);
close all;
%=============================================================================
