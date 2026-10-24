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
skip_if_display_cannot_place_figures('figure pick object');
% A mouse press resolves to the object drawn under the cursor: clicking a line
% sets CurrentObject to that line and fires its ButtonDownFcn, not the axes'.
% Geometry: axes box [40 40 320 200] px in a 400x300 figure, XLim [0 10],
% YLim [0 100], so data (5,50) is at pixel (200,140). A line from (0,0) to
% (10,100) passes through it. __figure_mouse_event__ replays via the web entry.
%=============================================================================
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
h = line('Parent', ax, 'XData', [0 10], 'YData', [0 100]);
drawnow();
global PICK_SEEN
PICK_SEEN = {};
h.ButtonDownFcn = @(src, evt) record_pick('line');
ax.ButtonDownFcn = @(src, evt) record_pick('axes');
f.ButtonDownFcn = @(src, evt) record_pick('figure');
%=============================================================================
% click on the line: CurrentObject is the line, the line callback fires
__figure_mouse_event__(f, 'press', [200 140], 'web');
drawnow();
pause(0.2);
assert_isequal(f.CurrentObject, h);
assert_isequal(numel(PICK_SEEN), 1);
assert_isequal(PICK_SEEN{1}, 'line');
%=============================================================================
% click on an empty area inside the axes: no object is hit, so the click
% resolves to the axes (its callback fires, not the line's)
PICK_SEEN = {};
__figure_mouse_event__(f, 'press', [72 220], 'web');
drawnow();
pause(0.2);
assert_isequal(f.CurrentObject, ax);
assert_isequal(numel(PICK_SEEN), 1);
assert_isequal(PICK_SEEN{1}, 'axes');
%=============================================================================
% HitTest off: the line is not pickable, the click passes through it to the axes
PICK_SEEN = {};
h.HitTest = 'off';
__figure_mouse_event__(f, 'press', [200 140], 'web');
drawnow();
pause(0.2);
assert_isequal(f.CurrentObject, ax);
assert_isequal(numel(PICK_SEEN), 1);
assert_isequal(PICK_SEEN{1}, 'axes');
h.HitTest = 'on';
close(f);
clear global PICK_SEEN
%=============================================================================
% topmost wins: the line drawn last is picked where both overlap
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
h1 = line('Parent', ax, 'XData', [0 10], 'YData', [50 50]);
h2 = line('Parent', ax, 'XData', [0 10], 'YData', [50 50]);
drawnow();
__figure_mouse_event__(f, 'press', [200 140], 'web');
assert_isequal(f.CurrentObject, h2);
close(f);
%=============================================================================
% patch: a filled polygon is picked when the click is inside it, not outside
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 10];
p = patch('Parent', ax, 'XData', [2 8 8 2], 'YData', [2 2 8 8], 'FaceColor', [0 0.5 1]);
drawnow();
% a click inside the polygon picks the patch; a click outside does not (so a
% second pickable object there would win instead)
q = patch('Parent', ax, 'XData', [0.5 1.5 1.5 0.5], 'YData', [0.5 0.5 1.5 1.5], ...
  'FaceColor', [1 0 0]);
drawnow();
__figure_mouse_event__(f, 'press', [200 140], 'web');   % data (5,5): inside p only
assert_isequal(f.CurrentObject, p);
__figure_mouse_event__(f, 'press', [72 60], 'web');      % data (1,1): inside q only
assert_isequal(f.CurrentObject, q);
close(f);
%=============================================================================
% image: the pixel rectangle is picked at its center
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 5];
ax.YLim = [0 5];
im = image('Parent', ax, 'CData', reshape(1:16, 4, 4));
drawnow();
% click the image centre, mapped through the axes' actual limits
cx = 40 + (2.5 - ax.XLim(1)) / (ax.XLim(2) - ax.XLim(1)) * 320;
cy = 40 + (2.5 - ax.YLim(1)) / (ax.YLim(2) - ax.YLim(1)) * 200;
__figure_mouse_event__(f, 'press', [cx cy], 'web');
assert_isequal(f.CurrentObject, im);
close(f);
%=============================================================================
% surface: a grid cell is picked (flat quad, viewed top-down)
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 3];
ax.YLim = [0 3];
s = surface('Parent', ax, 'XData', [1 2; 1 2], 'YData', [1 1; 2 2], 'ZData', [0 0; 0 0]);
view(ax, 2);
drawnow();
% click the quad centre (1.5,1.5), mapped through the axes' actual limits
cx = 40 + (1.5 - ax.XLim(1)) / (ax.XLim(2) - ax.XLim(1)) * 320;
cy = 40 + (1.5 - ax.YLim(1)) / (ax.YLim(2) - ax.YLim(1)) * 200;
__figure_mouse_event__(f, 'press', [cx cy], 'web');
assert_isequal(f.CurrentObject, s);
close(f);
%=============================================================================
% text: the extent rectangle is picked at its center
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 10];
t = text(5, 5, 'Nelson', 'Parent', ax, 'FontSize', 20);
drawnow();
ext = t.Extent;   % [left bottom width height] in data units
cx = 40 + (ext(1) + ext(3) / 2) / 10 * 320;
cy = 40 + (ext(2) + ext(4) / 2) / 10 * 200;
__figure_mouse_event__(f, 'press', [cx cy], 'web');
assert_isequal(f.CurrentObject, t);
close(f);
%=============================================================================
% scatter: a click on a marker picks the scatter series
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
sc = scatter(ax, [2 5 8], [2 5 8]);
ax.XLim = [0 10];
ax.YLim = [0 10];
drawnow();
__figure_mouse_event__(f, 'press', [200 140], 'web');   % data (5,5): on a marker
assert_isequal(f.CurrentObject, sc);
close(f);
%=============================================================================
% bubble chart: a click on a bubble picks the bubble chart
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
bc = bubblechart(ax, [2 5 8], [2 5 8], [10 20 30]);
ax.XLim = [0 10];
ax.YLim = [0 10];
drawnow();
__figure_mouse_event__(f, 'press', [200 140], 'web');   % data (5,5): on a bubble
assert_isequal(f.CurrentObject, bc);
close(f);
%=============================================================================
% a click outside every axes resolves to the figure
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
line('Parent', ax, 'XData', [0 10], 'YData', [0 100]);
drawnow();
global PICK_SEEN
PICK_SEEN = {};
f.ButtonDownFcn = @(src, evt) record_pick('figure');
ax.ButtonDownFcn = @(src, evt) record_pick('axes');
__figure_mouse_event__(f, 'press', [10 10], 'web');     % outside the axes box
drawnow();
pause(0.2);
assert_isequal(numel(PICK_SEEN), 1);
assert_isequal(PICK_SEEN{1}, 'figure');
close(f);
clear global PICK_SEEN
%=============================================================================
% native figure window input path: clicking the line selects it, an empty
% area falls back to the axes
is_portable = strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm();
if ~is_portable
  f = figure('Position', [100 100 400 300]);
  ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
  ax.XLim = [0 10];
  ax.YLim = [0 100];
  h = line('Parent', ax, 'XData', [0 10], 'YData', [0 100]);
  drawnow();
  __figure_mouse_event__(f, 'press', [200 140], 'native');
  assert_isequal(f.CurrentObject, h);
  __figure_mouse_event__(f, 'press', [72 220], 'native');
  assert_istrue(~isequal(f.CurrentObject, h));
  close(f);
end
%=============================================================================
function record_pick(name)
  global PICK_SEEN
  PICK_SEEN{end + 1} = name;
end
%=============================================================================
