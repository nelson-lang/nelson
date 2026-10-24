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
% An axes held by a uipanel or a uitab is painted by the container. Painting
% it must not mark the figure dirty again: otherwise every drawnow, even with
% nothing changed, repaints the whole container content.
% The containers must still repaint when a child really changes.
%=============================================================================
close all
%=============================================================================
% idle drawnow costs nothing once the scene is painted
%=============================================================================
fig = uifigure('Position', [50 50 1180 745]);
panelA = uipanel(fig, 'Position', [10 10 560 340]);
panelB = uipanel(fig, 'Position', [590 10 560 340]);
tg = uitabgroup(fig, 'Position', [10 370 560 340]);
tab = uitab(tg, 'Title', 'A');
parents = {panelA, panelB, tab};
for k = 1:numel(parents)
  ax = uiaxes(parents{k}, 'Position', [10 10 520 280]);
  plot(ax, 1:1500, rand(1, 1500));
end
for k = 1:10
  drawnow();
  pause(0.02);
end
N = 10;
tic();
for k = 1:N
  drawnow();
end
idleMs = 1000 * toc() / N;
close(fig);
% re-painting the three axes costs hundreds of ms per drawnow
assert_istrue(idleMs < 10);
%=============================================================================
% a property change still reaches the screen: uipanel
%=============================================================================
fig = figure('Visible', 'on', 'Position', [120 120 600 400], 'Color', [0 0 0]);
p = uipanel('Parent', fig, 'Units', 'normalized', 'Position', [0 0 1 1]);
ax = axes('Parent', p, 'Position', [0.1 0.1 0.8 0.8]);
l = line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
drawnow();
for k = 1:5
  drawnow();
end
frame = getframe(fig);
assert_istrue(count_green_pixels(frame.cdata) > 200);
l.Color = [0 0 1];
drawnow();
frame = getframe(fig);
assert_isequal(count_green_pixels(frame.cdata), 0);
assert_istrue(count_blue_pixels(frame.cdata) > 200);
close(fig);
%=============================================================================
% a property change still reaches the screen: uitab, and tab switch
%=============================================================================
fig = figure('Visible', 'on', 'Position', [120 120 600 400]);
g = uitabgroup('Parent', fig, 'Units', 'normalized', 'Position', [0 0 1 1]);
first = uitab('Parent', g, 'Title', 'first');
second = uitab('Parent', g, 'Title', 'second');
axFirst = axes('Parent', first, 'Position', [0.1 0.1 0.8 0.8]);
l = line('Parent', axFirst, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
axSecond = axes('Parent', second, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', axSecond, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 0 1], 'LineWidth', 4);
drawnow();
for k = 1:5
  drawnow();
end
frame = getframe(fig);
assert_istrue(count_green_pixels(frame.cdata) > 200);
l.Color = [1 0 0];
drawnow();
frame = getframe(fig);
assert_isequal(count_green_pixels(frame.cdata), 0);
assert_istrue(count_red_pixels(frame.cdata) > 200);
g.SelectedTab = second;
drawnow();
frame = getframe(fig);
assert_isequal(count_red_pixels(frame.cdata), 0);
assert_istrue(count_blue_pixels(frame.cdata) > 200);
close(fig);
%=============================================================================
% off screen capture (web raster path) still draws the container content
%=============================================================================
fig = figure('Visible', 'off', 'Position', [10 10 600 400]);
p = uipanel('Parent', fig, 'Units', 'normalized', 'Position', [0 0 1 1]);
ax = axes('Parent', p, 'Position', [0.1 0.1 0.8 0.8]);
l = line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
drawnow();
frame = getframe(fig);
assert_istrue(count_green_pixels(frame.cdata) > 200);
l.Color = [0 0 1];
drawnow();
frame = getframe(fig);
close(fig);
assert_isequal(count_green_pixels(frame.cdata), 0);
assert_istrue(count_blue_pixels(frame.cdata) > 200);
%=============================================================================
function n = count_green_pixels(img)
  n = nnz(img(:, :, 1) < 60 & img(:, :, 2) > 180 & img(:, :, 3) < 60);
end
%=============================================================================
function n = count_blue_pixels(img)
  n = nnz(img(:, :, 1) < 60 & img(:, :, 2) < 60 & img(:, :, 3) > 180);
end
%=============================================================================
function n = count_red_pixels(img)
  n = nnz(img(:, :, 1) > 180 & img(:, :, 2) < 60 & img(:, :, 3) < 60);
end
%=============================================================================
