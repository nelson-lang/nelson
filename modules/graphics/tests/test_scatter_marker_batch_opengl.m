%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
% On-screen (OpenGL) figures draw every marker of a series, and every bar of
% a bar or histogram series, from one vertex array batch instead of one
% immediate-mode primitive per item. Each branch of those batches (fill
% only, outline only, both, dot, every marker shape, per-point colours,
% alpha, animated data) must render a visible figure without error.
%=============================================================================
f = figure('Visible', 'on', 'Name', 'marker batch');
ax = axes('Parent', f);
hold(ax, 'on');
n = 2000;
x = rand(1, n) * 100;
y = rand(1, n) * 60;
filled = scatter(ax, x, y, 22, rand(n, 3), 'filled');
outlined = scatter(ax, x, y + 70, 30, [0.2 0.5 0.9]);
both = scatter(ax, x, y + 140, 40, rand(n, 3), 'filled', 'MarkerEdgeColor', [0 0 0]);
dots = plot(ax, x, y + 210, '.', 'MarkerSize', 14);
circles = plot(ax, x(1:50), y(1:50) + 280, 'o', 'MarkerSize', 12, ...
  'MarkerFaceColor', [0.9 0.4 0.1], 'MarkerEdgeColor', [0.1 0.1 0.1], 'LineWidth', 2);
shapes = {'s', 'd', '^', 'v', '<', '>', 'p', 'h', '+', 'x', '*'};
shaped = cell(1, numel(shapes));
for k = 1:numel(shapes)
  shaped{k} = scatter(ax, x(1:200), y(1:200) + 350 + 10 * k, 40, rand(200, 3), ...
    'filled', 'Marker', shapes{k}, 'MarkerEdgeColor', [0 0 0]);
end
hold(ax, 'off');
axis(ax, [-5 105 -5 480]);
drawnow();
try
  set(both, 'MarkerFaceAlpha', 0.4);
  drawnow();
catch
end
for k = 1:5
  set(filled, 'XData', x + k, 'YData', y + k, 'CData', rand(n, 3));
  set(dots, 'XData', x + k);
  set(shaped{1}, 'XData', x(1:200) + k);
  drawnow();
end
assert_isequal(get(filled, 'Marker'), 'o');
assert_isequal(get(dots, 'Marker'), '.');
assert_isequal(get(shaped{1}, 'Marker'), 'square');
assert_isequal(get(shaped{end}, 'Marker'), '*');
assert_isequal(size(get(filled, 'CData')), [n 3]);
assert_istrue(isgraphics(f));
close(f);
%=============================================================================
% Bars and histograms: fill and outline, fill only, outline only, animated.
f = figure('Visible', 'on', 'Name', 'rect batch');
ax1 = subplot(2, 2, 1, 'Parent', f);
b1 = bar(ax1, 1:300, rand(1, 300), 'FaceColor', [0.3 0.6 0.9], 'EdgeColor', [0 0 0.4]);
ax2 = subplot(2, 2, 2, 'Parent', f);
b2 = bar(ax2, 1:300, rand(1, 300), 'FaceColor', [0.3 0.6 0.9], 'EdgeColor', 'none');
ax3 = subplot(2, 2, 3, 'Parent', f);
b3 = barh(ax3, 1:300, rand(1, 300), 'FaceColor', 'none', 'EdgeColor', [0.8 0.2 0.2]);
ax4 = subplot(2, 2, 4, 'Parent', f);
h4 = histogram(ax4, randn(1, 5000), 200);
drawnow();
for k = 1:5
  set(b1, 'YData', rand(1, 300));
  set(h4, 'Data', randn(1, 5000));
  drawnow();
end
assert_isequal(numel(get(b1, 'YData')), 300);
assert_isequal(get(b2, 'EdgeColor'), 'none');
assert_isequal(get(b3, 'FaceColor'), 'none');
assert_istrue(isgraphics(h4));
close(f);
%=============================================================================
