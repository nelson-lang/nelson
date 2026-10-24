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
% An axes held by a uipanel is painted by the container through the QPainter
% renderer. Stroking a long, self-intersecting polyline there in one piece
% cost 0.7 s for 2000 points and 5.5 s for 20000 points per repaint; it is
% now stroked in bounded pieces, which must still join and dash seamlessly.
%=============================================================================
close all
%=============================================================================
% a real change on a dense line repaints in bounded time
%=============================================================================
fig = uifigure('Position', [100 100 640 480]);
p = uipanel(fig, 'Position', [10 10 620 460]);
ax = uiaxes(p, 'Position', [10 10 590 420]);
h = plot(ax, 1:20000, rand(1, 20000), 'LineWidth', 3);
drawnow();
drawnow();
elapsed = zeros(1, 3);
for k = 1:numel(elapsed)
  tic();
  h.Color = rand(1, 3);
  drawnow();
  elapsed(k) = toc();
end
close(fig);
% about 0.2 s here, 5.5 s when stroked in one piece
assert_istrue(median(elapsed) < 1.5);
%=============================================================================
% a sawtooth keeps every tip: no notch where two pieces meet. A thin pen is
% drawn segment by segment, a thick one in joined pieces.
%=============================================================================
check_sawtooth(0.5);
check_sawtooth(8);
%=============================================================================
% a dashed line keeps its dash phase across pieces: equal dashes and gaps
%=============================================================================
x = linspace(0, 1, 400);
y = 0.5 * ones(size(x));
image = capture_panel_line(x, y, [-0.05 1.05], [0 1], {'LineWidth', 2, 'LineStyle', '--'});
green = is_green(image);
[~, row] = max(sum(green, 2));
runs = run_lengths(green(row, :));
dashes = runs(1:2:end);
gaps = runs(2:2:end);
% first and last dash are clipped by the axes limits
dashes = dashes(2:(end - 1));
assert_istrue(numel(dashes) > 5);
assert_istrue(max(dashes) - min(dashes) <= 2);
assert_istrue(max(gaps) - min(gaps) <= 2);
%=============================================================================
function image = capture_panel_line(x, y, xLimits, yLimits, options)
  % a visible panel: its widget paints the axes, and getframe grabs that paint
  f = uifigure('Position', [100 100 600 400]);
  p = uipanel(f, 'Position', [0 0 600 400], 'BackgroundColor', [0 0 0]);
  ax = uiaxes(p, 'Position', [20 20 560 360], 'Color', [0 0 0], ...
    'XLim', xLimits, 'YLim', yLimits, 'Visible', 'off');
  line('Parent', ax, 'XData', x, 'YData', y, 'Color', [0 1 0], options{:});
  drawnow();
  drawnow();
  frame = getframe(f);
  close(f);
  image = frame.cdata;
end
%=============================================================================
function mask = is_green(image)
  mask = (image(:, :, 2) > 128) & (image(:, :, 1) < 100) & (image(:, :, 3) < 100);
end
%=============================================================================
function runs = run_lengths(values)
  % lengths of the alternating true/false runs, starting at the first true
  first = find(values, 1);
  last = find(values, 1, 'last');
  values = values(first:last);
  changes = [find(diff(values) ~= 0), numel(values)];
  runs = diff([0, changes]);
end
%=============================================================================
function check_sawtooth(lineWidth)
  teeth = 20;
  segments = 2 * teeth;
  x = 0:segments;
  y = mod(x, 2);
  % the y range is flat enough that a missing join shows at the tips
  xLimits = [-8 48];
  yLimits = [-6 7];
  image = capture_panel_line(x, y, xLimits, yLimits, {'LineWidth', lineWidth});
  green = is_green(image);
  [~, cols] = find(green);
  assert_istrue(~isempty(cols));
  left = min(cols);
  right = max(cols);
  % the line is continuous: every column it spans holds some ink
  assert_isequal(sum(any(green(:, left:right), 1)), right - left + 1);
  % all the tips of a side reach the same row: a stray cap raises one
  spacing = (right - left) / segments;
  tips = zeros(1, segments - 1);
  for k = 1:(segments - 1)
    center = round(left + k * spacing);
    windowRows = find(any(green(:, (center - 2):(center + 2)), 2));
    if mod(k, 2) == 1
      tips(k) = min(windowRows);
    else
      tips(k) = max(windowRows);
    end
  end
  assert_istrue(max(tips(1:2:end)) - min(tips(1:2:end)) <= 1);
  assert_istrue(max(tips(2:2:end)) - min(tips(2:2:end)) <= 1);
  % one more (collinear) vertex in the first segment moves every boundary
  % between pieces by one vertex, not the drawing: a seam would show up
  shifted = capture_panel_line([0, 0.5, x(2:end)], [0, 0.5, y(2:end)], ...
    xLimits, yLimits, {'LineWidth', lineWidth});
  firstTooth = round(left + 2 * spacing);
  difference = abs(double(image(:, firstTooth:end, 2)) - double(shifted(:, firstTooth:end, 2)));
  assert_isequal(nnz(difference > 64), 0);
end
%=============================================================================
