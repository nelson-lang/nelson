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
% Dashed and dash-dot lines must repeat with a short stroke period of about
% eight pen widths, the same period the reference stroke uses. An earlier
% pattern drew them roughly twice (dashed) and three times (dash-dot) as long,
% which reads as visibly stretched dashes. The dotted style keeps its own short
% dot and the solid style stays unbroken.
%
% The period is measured on the rendered stroke and expressed in units of the
% rendered pen width, so the check does not depend on the display resolution.
%=============================================================================
reset(groot());
close all;
%=============================================================================
PERIOD_LOW = 4.5;    % pen widths; the reference period is ~8
PERIOD_HIGH = 11.5;  % the old dashed period was ~17, dash-dot ~24
%=============================================================================
% The RenderWeb raster (saveas to PNG) draws the pattern at a wide pen, where
% every dash and gap is several pixels: the cleanest place to measure a period.
for lineWidth = [1 2 3]
  dashedRatio = saveas_period('--', lineWidth);
  asserts.istrue(dashedRatio >= PERIOD_LOW);
  asserts.istrue(dashedRatio <= PERIOD_HIGH);

  dashDotRatio = saveas_period('-.', lineWidth);
  asserts.istrue(dashDotRatio >= PERIOD_LOW);
  asserts.istrue(dashDotRatio <= PERIOD_HIGH);
end
%=============================================================================
% A dotted line keeps its own short dot (~2 pen widths), unchanged.
dottedRatio = saveas_period(':', 3);
asserts.istrue(dottedRatio >= 1.2);
asserts.istrue(dottedRatio < 4);
%=============================================================================
% A solid line paints one unbroken run: no gap as wide as the pen.
[solidRow, solidPen] = render_line_row_saveas('-', 3);
[~, ~, solidOff] = row_runs(solidRow);
asserts.istrue(isempty(solidOff) || max(solidOff) < solidPen);
%=============================================================================
% The software QPainter path (getframe on a hidden figure) must draw the dashed
% period just as short.
[dashRow, dashPen] = render_line_row_getframe('--', 3);
[onStart, onLen, ~] = row_runs(dashRow);
asserts.istrue(numel(onStart) >= 6);
pitch = median(diff(onStart));
asserts.istrue(pitch / dashPen >= PERIOD_LOW);
asserts.istrue(pitch / dashPen <= PERIOD_HIGH);
%=============================================================================
close all;
%=============================================================================
function ratio = saveas_period(style, lineWidth)
  [row, penWidth] = render_line_row_saveas(style, lineWidth);
  [onStart, onLen, ~] = row_runs(row);
  if strcmp(style, '-.')
    % A dash-dot cycle alternates a long dash and a short one. Keep only the
    % long dashes (longer than the median run); the distance between two
    % consecutive ones is the full period.
    starts = onStart(onLen > median(onLen));
  else
    starts = onStart;
  end
  ratio = median(diff(starts)) / penWidth;
end
%=============================================================================
function [row, penWidth] = render_line_row_saveas(style, lineWidth)
  f = figure('Visible', 'off', 'Position', [100 100 900 220], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Units', 'normalized', 'Position', [0 0.4 1 0.2], 'Color', [1 1 1]);
  t = linspace(0, 1, 200);
  line('Parent', ax, 'XData', t, 'YData', zeros(size(t)), 'LineStyle', style, ...
       'LineWidth', lineWidth, 'Color', [0 0 0]);
  set(ax, 'XLim', [0 1], 'YLim', [-1 1], 'Visible', 'off');
  drawnow();
  filename = [tempname(), '.png'];
  saveas(f, filename);
  img = imread(filename);
  rmfile(filename);
  close(f);
  [row, penWidth] = line_row(img);
end
%=============================================================================
function [row, penWidth] = render_line_row_getframe(style, lineWidth)
  f = figure('Visible', 'off', 'Position', [100 100 900 220], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Units', 'normalized', 'Position', [0 0.4 1 0.2], 'Color', [1 1 1]);
  t = linspace(0, 1, 200);
  line('Parent', ax, 'XData', t, 'YData', zeros(size(t)), 'LineStyle', style, ...
       'LineWidth', lineWidth, 'Color', [0 0 0]);
  set(ax, 'XLim', [0 1], 'YLim', [-1 1], 'Visible', 'off');
  drawnow();
  img = getframe(f).cdata;
  close(f);
  [row, penWidth] = line_row(img);
end
%=============================================================================
function [row, penWidth] = line_row(img)
  dark = double(img(:, :, 1)) < 128;
  counts = sum(dark, 2);
  [~, r] = max(counts);
  row = dark(r, :);
  % Pen width: the median vertical thickness sampled at the dash centers.
  [onStart, onLen, ~] = row_runs(row);
  thickness = [];
  for k = 1:numel(onStart)
    xc = onStart(k) + floor(onLen(k) / 2);
    thickness(end + 1) = sum(dark(:, xc));
  end
  penWidth = max(1, median(thickness));
end
%=============================================================================
function [onStart, onLen, offLen] = row_runs(row)
  d = diff([0, double(row), 0]);
  onStart = find(d == 1);
  onEnd = find(d == -1) - 1;
  onLen = onEnd - onStart + 1;
  keep = onLen >= 2;                 % drop single anti-aliased pixels
  onStart = onStart(keep);
  onEnd = onEnd(keep);
  onLen = onLen(keep);
  if numel(onStart) >= 2
    offLen = onStart(2:end) - onEnd(1:end-1) - 1;
  else
    offLen = [];
  end
end
%=============================================================================
