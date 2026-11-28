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
% A point is 96/72 pixel whatever the screen dpi and the render path.
%=============================================================================
% a FontUnits change converts FontSize and keeps the size of the drawn text
f = figure('Position', [100 100 500 300], 'Color', [1 1 1]);
ax = axes('Parent', f, 'Position', [0 0 1 1], 'Visible', 'off', 'XLim', [0 1], 'YLim', [0 1]);
t = text(0.5, 0.5, 'HHHH', 'Parent', ax, 'FontSize', 30, 'Color', [0 0 0], ...
  'HorizontalAlignment', 'center');
drawnow();
inkPoints = inkSize(f);
t.FontUnits = 'pixels';
assert_isapprox(t.FontSize, 40, 1e-9);
drawnow();
assert_istrue(all(abs(inkSize(f) - inkPoints) <= 2));
close(f);
%=============================================================================
% a marker of an axes in a uipanel has the same size in a shown and in a hidden figure
sizes = zeros(2, 2);
visible = {'on', 'off'};
for k = 1:2
  f = figure('Visible', visible{k}, 'Position', [100 100 500 300], 'Color', [1 1 1]);
  p = uipanel('Parent', f, 'Position', [0 0 1 1], 'BackgroundColor', [1 1 1], ...
    'BorderType', 'none');
  ax = axes('Parent', p, 'Position', [0 0 1 1], 'Visible', 'off', 'XLim', [0 1], 'YLim', [0 1]);
  line(0.5, 0.5, 'Parent', ax, 'Marker', 's', 'MarkerSize', 30, 'MarkerFaceColor', [0 0 0], ...
    'MarkerEdgeColor', [0 0 0], 'LineStyle', 'none');
  drawnow();
  sizes(k, :) = inkSize(f);
  close(f);
end
assert_istrue(all(abs(sizes(1, :) - sizes(2, :)) <= 3));
% the square is drawn smaller than its 30 points (40 pixels) box
assert_istrue(all(sizes(:) > 25 & sizes(:) < 42));
%=============================================================================
% the frame of a figure, shown or hidden, scales text, lines and markers together, whatever
% the display scale: sizes are compared in figure pixels
figureSize = [500 300];
for k = 1:2
  [f, ax] = bareFigure(visible{k}, figureSize);
  t = text(0.5, 0.5, 'HHHH', 'Parent', ax, 'FontSize', 30, 'Color', [0 0 0], ...
    'HorizontalAlignment', 'center');
  ink = figurePixels(f, figureSize);
  t.Units = 'pixels';
  extent = t.Extent;
  assert_istrue(ink(1) > 0.85 * extent(3) && ink(1) < 1.05 * extent(3));
  close(f);
  [f, ax] = bareFigure(visible{k}, figureSize);
  line([0.2 0.8], [0.5 0.5], 'Parent', ax, 'LineWidth', 9, 'Color', [0 0 0]);
  ink = figurePixels(f, figureSize);
  assert_istrue(abs(ink(2) - 9 * 96 / 72) <= 1.2);
  close(f);
  [f, ax] = bareFigure(visible{k}, figureSize);
  line(0.5, 0.5, 'Parent', ax, 'Marker', 's', 'MarkerSize', 30, 'MarkerFaceColor', [0 0 0], ...
    'MarkerEdgeColor', [0 0 0], 'LineStyle', 'none');
  ink = figurePixels(f, figureSize);
  assert_istrue(all(abs(ink - sizes(2, :)) <= 3));
  close(f);
end
%=============================================================================
function [f, ax] = bareFigure(visible, figureSize)
  f = figure('Visible', visible, 'Position', [100 100 figureSize], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Position', [0 0 1 1], 'Visible', 'off', 'XLim', [0 1], 'YLim', [0 1]);
end
%=============================================================================
% ink size of the capture, brought back to the pixels of the figure
function wh = figurePixels(f, figureSize)
  drawnow();
  frame = getframe(f);
  ink = all(frame.cdata < 128, 3);
  [rows, cols] = find(ink);
  assert_isfalse(isempty(cols));
  wh = [max(cols) - min(cols), max(rows) - min(rows)] + 1;
  wh = wh .* figureSize ./ [size(frame.cdata, 2), size(frame.cdata, 1)];
end
%=============================================================================
function wh = inkSize(f)
  frame = getframe(f);
  ink = all(frame.cdata < 128, 3);
  [rows, cols] = find(ink);
  assert_isfalse(isempty(cols));
  wh = [max(cols) - min(cols), max(rows) - min(rows)] + 1;
end
%=============================================================================
