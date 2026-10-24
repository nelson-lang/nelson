%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% A line drawn with LineStyle 'none' must still paint its markers: the axes
% data-limit clip box was tested against the pixel coordinates markers are
% drawn in, which dropped every marker built out of line segments.
% A hidden figure is captured by the software RenderQt path, which is where the
% clip box is applied; a visible one is grabbed from the OpenGL widget instead.
%=============================================================================
reset(groot());
close all;
%=============================================================================
AXES_POSITION = [0.1 0.1 0.8 0.8];
XLIM = [0 4];
YLIM = [0 2];
XDATA = [1 2 3];
YDATA = [1 1 1];
%=============================================================================
% Every marker of the standard set must show up, painted in its edge color.
symbols = {'o', '+', '*', '.', 'x', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};
for k = 1:numel(symbols)
  img = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, symbols{k}, ...
                           [0 1 0], [1 0 0]);
  asserts.istrue(count_color(img, 'red') > 0);
end
%=============================================================================
% A closed marker also fills its face, in a color of its own.
filledSymbols = {'o', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};
for k = 1:numel(filledSymbols)
  img = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, filledSymbols{k}, ...
                           [0 1 0], [1 0 0]);
  asserts.istrue(count_color(img, 'green') > 0);
end
%=============================================================================
% Marker 'none' paints nothing at all.
img = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, 'none', [0 1 0], [1 0 0]);
asserts.isequal(count_color(img, 'red'), 0);
asserts.isequal(count_color(img, 'green'), 0);
%=============================================================================
% Markers land on their data points, not somewhere else in the axes.
img = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, '^', [0 1 0], [1 0 0]);
mask = color_mask(img, 'green');
height = size(img, 1);
width = size(img, 2);
for k = 1:numel(XDATA)
  column = round((AXES_POSITION(1) + AXES_POSITION(3) * (XDATA(k) - XLIM(1)) / (XLIM(2) - XLIM(1))) * width);
  fromBottom = (AXES_POSITION(2) + AXES_POSITION(4) * (YDATA(k) - YLIM(1)) / (YLIM(2) - YLIM(1))) * height;
  row = round(height - fromBottom);
  window = mask(max(1, row - 6):min(height, row + 6), max(1, column - 6):min(width, column + 6));
  asserts.istrue(nnz(window) > 0);
end
% and nowhere else: all the green belongs to the three markers.
asserts.isequal(nnz(mask(1:round(0.25 * height), :)), 0);
%=============================================================================
% A bigger marker paints more of them.
small = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, '^', [0 1 0], [1 0 0], 6);
big = render_marker_line(AXES_POSITION, XLIM, YLIM, XDATA, YDATA, '^', [0 1 0], [1 0 0], 18);
asserts.istrue(count_color(big, 'green') > count_color(small, 'green'));
%=============================================================================
% plot() honors the marker of a linespec the same way.
f = figure('Visible', 'off', 'Position', [100 100 600 400], 'Color', [1 1 1]);
ax = axes('Parent', f, 'Units', 'normalized', 'Position', AXES_POSITION, 'Color', [1 1 1]);
plot(ax, XDATA, YDATA, '^', 'MarkerSize', 14, 'MarkerFaceColor', [0 1 0], ...
     'MarkerEdgeColor', [1 0 0]);
set(ax, 'XLim', XLIM, 'YLim', YLIM);
drawnow();
img = getframe(f).cdata;
close(f);
asserts.istrue(count_color(img, 'green') > 0);
asserts.istrue(count_color(img, 'red') > 0);
%=============================================================================
close all;
%=============================================================================
function img = render_marker_line(axesPosition, xLimits, yLimits, xData, yData, symbol, faceColor, edgeColor, markerSize)
  if nargin < 9
    markerSize = 14;
  end
  f = figure('Visible', 'off', 'Position', [100 100 600 400], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Units', 'normalized', 'Position', axesPosition, 'Color', [1 1 1]);
  set(ax, 'XLim', xLimits, 'YLim', yLimits, 'XTick', [], 'YTick', []);
  line('Parent', ax, 'XData', xData, 'YData', yData, 'LineStyle', 'none', ...
       'Marker', symbol, 'MarkerSize', markerSize, 'MarkerFaceColor', faceColor, ...
       'MarkerEdgeColor', edgeColor);
  set(ax, 'XLim', xLimits, 'YLim', yLimits);
  drawnow();
  img = getframe(f).cdata;
  close(f);
end
%=============================================================================
function mask = color_mask(img, name)
  R = double(img(:, :, 1));
  G = double(img(:, :, 2));
  B = double(img(:, :, 3));
  if strcmp(name, 'red')
    mask = R > 150 & G < 100 & B < 100;
  else
    mask = G > 150 & R < 100 & B < 100;
  end
end
%=============================================================================
function n = count_color(img, name)
  n = nnz(color_mask(img, name));
end
%=============================================================================
