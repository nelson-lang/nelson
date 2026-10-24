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
skip_if_portable_graphics_backend('native mouse motion events are not available.');
% Pointer motion without any button pressed runs WindowButtonMotionFcn and
% updates CurrentPoint, whatever the window content. __figure_mouse_event__
% delivers the event to the widget under the point, like the real pointer.
%=============================================================================
global MOTION_SEEN
%=============================================================================
% empty figure
MOTION_SEEN = {};
f = figure('Position', [300 300 400 300], 'WindowButtonMotionFcn', @record_motion);
drawnow();
assert_istrue(move_and_flush(f, [219 200]));
assert_isapprox(f.CurrentPoint, [219 200], 1e-9);
assert_isequal(numel(MOTION_SEEN), 1);
assert_isapprox(MOTION_SEEN{1}, [219 200], 1e-9);
%=============================================================================
% the same figure once the axes canvas replaced the empty drawing area
ax = axes(f, 'Position', [40 / 400 40 / 300 320 / 400 200 / 300]);
ax.XLim = [0 10];
ax.YLim = [0 100];
drawnow();
MOTION_SEEN = {};
assert_istrue(move_and_flush(f, [280 190]));
assert_isapprox(f.CurrentPoint, [280 190], 1e-9);
assert_ax_cp(ax.CurrentPoint, [7.5 75], f, [10 100]);
assert_isequal(numel(MOTION_SEEN), 1);
%=============================================================================
% outside the axes box, then in normalized figure Units
assert_istrue(move_and_flush(f, [10 290]));
figurePixelSize = f.Position(3:4);
f.Units = 'normalized';
assert_istrue(move_and_flush(f, [200 150]));
expectedNorm = [200 150] ./ figurePixelSize;
normTol = 1e-9 + 2 ./ figurePixelSize;
assert_istrue(all(abs(f.CurrentPoint - expectedNorm) <= normTol));
assert_isequal(numel(MOTION_SEEN), 3);
assert_isapprox(MOTION_SEEN{2}, [10 290], 1e-9);
f.Units = 'pixels';
close(f);
%=============================================================================
% figure with a uicontrol: motion over the control reaches the figure too
MOTION_SEEN = {};
f = figure('Position', [300 300 400 300], 'WindowButtonMotionFcn', @record_motion);
uicontrol(f, 'Style', 'pushbutton', 'String', 'OK', 'Position', [10 10 100 40]);
drawnow();
assert_istrue(move_and_flush(f, [50 30]));
assert_istrue(move_and_flush(f, [300 250]));
assert_isequal(numel(MOTION_SEEN), 2);
assert_isapprox(MOTION_SEEN{1}, [50 30], 1e-9);
assert_isapprox(f.CurrentPoint, [300 250], 1e-9);
close(f);
%=============================================================================
% uifigure behaves like figure
MOTION_SEEN = {};
u = uifigure('Position', [300 300 400 300], 'WindowButtonMotionFcn', @record_motion);
drawnow();
assert_istrue(move_and_flush(u, [219 200]));
assert_isapprox(u.CurrentPoint, [219 200], 1e-9);
assert_isequal(numel(MOTION_SEEN), 1);
close(u);
%=============================================================================
% without WindowButtonMotionFcn, motion leaves CurrentPoint unchanged
f = figure('Position', [300 300 400 300]);
drawnow();
__figure_mouse_event__(f, 'press', [20 30]);
__figure_mouse_event__(f, 'motion', [219 200]);
assert_isapprox(f.CurrentPoint, [20 30], 1e-9);
close(f);
clear global MOTION_SEEN
%=============================================================================
function delivered = move_and_flush(fig, xy)
  delivered = __figure_mouse_event__(fig, 'motion', xy);
  drawnow();
  pause(0.2);
end
%=============================================================================
function record_motion(src, evt)
  global MOTION_SEEN
  MOTION_SEEN{end + 1} = src.CurrentPoint;
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
