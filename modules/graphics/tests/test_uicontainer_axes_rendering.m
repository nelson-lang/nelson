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
% Axes parented to a uitab or a uipanel must be DRAWN, not merely created:
% the container is a frame of its own and the render traversal has to enter it.
% Capture off screen ('Visible', 'off'), where nothing but that traversal can
% put the pixels on the canvas.
%=============================================================================
% axes inside a uitab
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
g = uitabgroup('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
t = uitab('Parent', g, 'Title', 'one');
ax = axes('Parent', t, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
drawnow();
frame = getframe(f);
close(f);
assert_istrue(count_green_pixels(frame.cdata) > 200);
%=============================================================================
% axes inside a uipanel
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
p = uipanel('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
ax = axes('Parent', p, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
drawnow();
frame = getframe(f);
close(f);
assert_istrue(count_green_pixels(frame.cdata) > 200);
%=============================================================================
% the graphics children land inside the container, not over the whole figure:
% a panel on the right half leaves the left half untouched
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400], 'Color', [0 0 0]);
p = uipanel('Parent', f, 'Units', 'normalized', 'Position', [0.5 0 0.5 1]);
ax = axes('Parent', p, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
drawnow();
frame = getframe(f);
close(f);
image = frame.cdata;
half = floor(size(image, 2) / 2);
assert_isequal(count_green_pixels(image(:, 1:half, :)), 0);
assert_istrue(count_green_pixels(image(:, (half + 1):end, :)) > 200);
%=============================================================================
% only the selected tab is drawn
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
g = uitabgroup('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
first = uitab('Parent', g, 'Title', 'first');
second = uitab('Parent', g, 'Title', 'second');
axFirst = axes('Parent', first, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', axFirst, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
axSecond = axes('Parent', second, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', axSecond, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 0 1], 'LineWidth', 4);
drawnow();
onFirst = getframe(f);
assert_istrue(count_green_pixels(onFirst.cdata) > 200);
assert_isequal(count_blue_pixels(onFirst.cdata), 0);
g.SelectedTab = second;
drawnow();
onSecond = getframe(f);
close(f);
assert_istrue(count_blue_pixels(onSecond.cdata) > 200);
assert_isequal(count_green_pixels(onSecond.cdata), 0);
%=============================================================================
% saveas goes through the export path: the same content must reach the file
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
g = uitabgroup('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
t = uitab('Parent', g, 'Title', 'one');
ax = axes('Parent', t, 'Position', [0.1 0.1 0.8 0.8]);
line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2, ...
  'Color', [0 1 0], 'LineWidth', 4);
imageName = [tempname(), '.png'];
saveas(f, imageName);
close(f);
assert_istrue(isfile(imageName));
exported = imread(imageName);
rmfile(imageName);
assert_istrue(count_green_pixels(exported) > 200);
%=============================================================================
function count = count_green_pixels(image)
  red = image(:, :, 1);
  green = image(:, :, 2);
  blue = image(:, :, 3);
  count = nnz((green > 180) & (red < 120) & (blue < 120));
end
%=============================================================================
function count = count_blue_pixels(image)
  red = image(:, :, 1);
  green = image(:, :, 2);
  blue = image(:, :, 3);
  count = nnz((blue > 180) & (red < 120) & (green < 120));
end
%=============================================================================
