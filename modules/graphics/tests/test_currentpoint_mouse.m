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
skip_if_portable_graphics_backend('native mouse event synthesis is not available.');
% Mouse events update figure and axes CurrentPoint before the callbacks run.
% __figure_mouse_event__ replays the event through the figure window input
% path ([x y]: pixels from the bottom-left corner of the drawing area).
%=============================================================================
f = figure('Position', [100 100 400 300]);
% normalized axes: the axes box is [40 40 320 200] pixels, whole device
% pixels at the usual display scales
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
drawnow();
assert_istrue(__figure_mouse_event__(f, 'press', [200 140]));
assert_isapprox(f.CurrentPoint, [200 140], 1e-9);
zl = ax.ZLim;
cp = ax.CurrentPoint;
assert_ax_cp(cp(1, :), [5 50], f, [10 100]);
assert_ax_cp(cp(2, :), [5 50], f, [10 100]);
assert_isapprox([cp(1, 3) cp(2, 3)], [zl(2) zl(1)], 1e-6);
assert_istrue(__figure_mouse_event__(f, 'release', [104 80]));
assert_isapprox(f.CurrentPoint, [104 80], 1e-9);
cp = ax.CurrentPoint;
assert_ax_cp(cp(1, :), [2 20], f, [10 100]);
assert_ax_cp(cp(2, :), [2 20], f, [10 100]);
assert_isapprox([cp(1, 3) cp(2, 3)], [zl(2) zl(1)], 1e-6);
%=============================================================================
% The press above followed by a release at a different point is a left-drag,
% which pans the 2-D axes (the default modeless interaction, like the reference).
% Panning changes the axes limits, so the remaining cases - which expect the
% original limits - continue on a fresh figure. This is correct behaviour, not
% a defect; the drag only shifts the limits enough to matter once the drawing
% area is rounded through device pixels (e.g. 298 px instead of 300 at 125 %).
close(f);
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
drawnow();
%=============================================================================
% outside the axes box: still extrapolated in data units
__figure_mouse_event__(f, 'press', [8 270]);
assert_ax_cp(ax.CurrentPoint, [-1 115], f, [10 100]);
%=============================================================================
% motion only updates CurrentPoint when WindowButtonMotionFcn is set (a
% motion to the same point is not an event, like for the real pointer)
__figure_mouse_event__(f, 'motion', [270 180]);
assert_isapprox(f.CurrentPoint, [8 270], 1e-9);
f.WindowButtonMotionFcn = @(src, evt) disp('');
__figure_mouse_event__(f, 'motion', [280 190]);
assert_isapprox(f.CurrentPoint, [280 190], 1e-9);
assert_ax_cp(ax.CurrentPoint, [7.5 75], f, [10 100]);
f.WindowButtonMotionFcn = [];
%=============================================================================
% reversed directions
ax.XDir = 'reverse';
__figure_mouse_event__(f, 'press', [104 140]);
assert_ax_cp(ax.CurrentPoint, [8 50], f, [10 100]);
ax.XDir = 'normal';
ax.YDir = 'reverse';
__figure_mouse_event__(f, 'press', [104 90]);
assert_ax_cp(ax.CurrentPoint, [2 75], f, [10 100]);
ax.YDir = 'normal';
%=============================================================================
% log scale
ax.YScale = 'log';
ax.YLim = [1 1000];
__figure_mouse_event__(f, 'press', [200, 40 + 200 * 2 / 3]);
lossYlog = abs(f.Position(4) - 300);
yFactor = 10 ^ (0.05 * lossYlog);
cpLog = ax.CurrentPoint(1, 1:2);
assert_ax_cp([cpLog(1) 100], [5 100], f, [10 0]);
assert_istrue(cpLog(2) >= 100 / yFactor - 1e-9 && cpLog(2) <= 100 * yFactor + 1e-9);
ax.YScale = 'linear';
ax.YLim = [0 100];
%=============================================================================
% zoomed limits
ax.XLim = [2 4];
__figure_mouse_event__(f, 'press', [200 140]);
assert_ax_cp(ax.CurrentPoint, [3 50], f, [2 100]);
%=============================================================================
% DataAspectRatio: the plot box shrinks to a 200x200 square centered in the
% 320x200 position
ax.XLim = [0 10];
ax.YLim = [0 10];
ax.DataAspectRatio = [1 1 1];
drawnow();
__figure_mouse_event__(f, 'press', [150 190]);
squareTol = 1e-9 + 10 * 4 * abs(f.Position(4) - 300) / 300;
assert_istrue(all(abs(ax.CurrentPoint(1, 1:2) - [2.5 7.5]) <= squareTol));
%=============================================================================
% figure Units
figurePixelSize = f.Position(3:4);
f.Units = 'normalized';
__figure_mouse_event__(f, 'press', [200 150]);
expectedNorm = [200 150] ./ figurePixelSize;
normTol = 1e-9 + 2 ./ figurePixelSize;
assert_istrue(all(abs(f.CurrentPoint - expectedNorm) <= normTol));
f.Units = 'pixels';
close(f);
%=============================================================================
% 3-D view: front and back points lie on the view line through the box center
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 1];
ax.YLim = [0 1];
ax.ZLim = [0 1];
view(ax, 3);
drawnow();
__figure_mouse_event__(f, 'press', [200 140]);
cp = ax.CurrentPoint;
assert_isequal(size(cp), [2 3]);
d = cp(2, :) - cp(1, :);
assert_istrue(norm(d) > 0.5);
c = [0.5 0.5 0.5] - cp(1, :);
colinearTol = 1e-9 + 0.05 * (abs(f.Position(3) - 400) + abs(f.Position(4) - 300));
assert_istrue(norm(cross(d, c)) / norm(d) < colinearTol);
close(f);
%=============================================================================
% callbacks see the updated CurrentPoint
f = figure('Position', [100 100 400 300]);
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
drawnow();
global CP_SEEN
CP_SEEN = {};
f.WindowButtonDownFcn = @(src, evt) record_current_point(src, ax);
__figure_mouse_event__(f, 'press', [104 80]);
drawnow();
pause(0.2);
assert_isequal(numel(CP_SEEN), 1);
assert_isapprox(CP_SEEN{1}{1}, [104 80], 1e-9);
assert_ax_cp(CP_SEEN{1}{2}, [2 20], f, [10 100]);
%=============================================================================
% web client entry point: same coordinates, same properties, same callbacks
CP_SEEN = {};
assert_istrue(__figure_mouse_event__(f, 'press', [360 240], 'web'));
drawnow();
pause(0.2);
assert_isapprox(f.CurrentPoint, [360 240], 1e-9);
cp = ax.CurrentPoint;
assert_ax_cp(cp(1, :), [10 100], f, [10 100]);
assert_ax_cp(cp(2, :), [10 100], f, [10 100]);
assert_isapprox([cp(1, 3) cp(2, 3)], [zl(2) zl(1)], 1e-6);
assert_isequal(numel(CP_SEEN), 1);
close(f);
clear global CP_SEEN
%=============================================================================
function record_current_point(fig, ax)
  global CP_SEEN
  CP_SEEN{end + 1} = {fig.CurrentPoint, ax.CurrentPoint};
end
%=============================================================================
function assert_ax_cp(value, expected, f, ranges)
  % Clicks are given in logical pixels. On a scaled display a shown drawing
  % area is rounded through device pixels (e.g. a height of 300 comes back as
  % 298 logical px at 125 %) and the axes box is rounded to whole pixels, so
  % the pixel -> data mapping drifts by a few pixels. Budget four pixels of box
  % error per axis, scaled by the figure's deviation from its request, so the
  % budget vanishes at 100 % scale where the mapping is exact.
  loss = abs(f.Position(3:4) - [400 300]);
  tol = 1e-9 + ranges .* (4 .* loss ./ [400 300]);
  v = value(1, 1:2);
  assert_istrue(all(abs(v - expected) <= tol));
end
%=============================================================================
