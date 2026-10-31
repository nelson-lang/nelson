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
% axes CurrentPoint of axes placed in containers: the pointer is converted
% through the container offsets.
%=============================================================================
f = figure('Position', [100 100 400 300]);
% pixel positions start at 1: the panel starts 20 pixels after the figure origin
p = uipanel(f, 'Units', 'pixels', 'Position', [21 21 360 260], 'BorderType', 'none');
ax = pixel_axes(p, [30 30 300 200]);
drawnow();
__figure_mouse_event__(f, 'press', [200 150]);
assert_isapprox(f.CurrentPoint, [200 150], 1e-9);
assert_isapprox(ax.CurrentPoint(1, 1:2), [5 50], 1e-9);
__figure_mouse_event__(f, 'press', [50 50]);
assert_isapprox(ax.CurrentPoint(1, 1:2), [0 0], 1e-9);
% nested panel
p2 = uipanel(p, 'Units', 'pixels', 'Position', [11 11 340 240], 'BorderType', 'none');
ax2 = pixel_axes(p2, [20 20 300 200]);
drawnow();
__figure_mouse_event__(f, 'press', [200 150]);
assert_isapprox(ax2.CurrentPoint(1, 1:2), [5 50], 1e-9);
close(f);
%=============================================================================
% uitab: the page starts at the bottom-left corner of the tab group (tabs on
% top)
f = figure('Position', [100 100 400 300]);
tg = uitabgroup(f, 'Units', 'pixels', 'Position', [1 1 400 300]);
t = uitab(tg, 'Title', 'A');
ax = pixel_axes(t, [50 50 300 150]);
drawnow();
__figure_mouse_event__(f, 'press', [200 125]);
assert_isapprox(ax.CurrentPoint(1, 1:2), [5 50], 1e-9);
close(f);
%=============================================================================
% tiledlayout: each tile maps the pointer in its own data space
f = figure('Position', [100 100 400 300]);
t = tiledlayout(f, 1, 2);
a1 = nexttile(t);
a1.XLim = [0 10];
a1.YLim = [0 10];
a2 = nexttile(t);
a2.XLim = [0 10];
a2.YLim = [0 10];
drawnow();
p1 = a1.Position;
__figure_mouse_event__(f, 'press', [(p1(1) + p1(3) / 2) * 400, (p1(2) + p1(4) / 2) * 300]);
assert_ax_cp(a1.CurrentPoint, [5 5], f, [10 10]);
assert_istrue(a2.CurrentPoint(1, 1) < 0);
close(f);
%=============================================================================
function ax = pixel_axes(parent, position)
  % Units before Position: the pixel Position must not be read as normalized
  ax = axes('Parent', parent);
  ax.Units = 'pixels';
  % Pixel positions start at 1: shift so the axes starts at the container offset.
  ax.Position = position + [1 1 0 0];
  ax.XLim = [0 10];
  ax.YLim = [0 100];
end
%=============================================================================
function assert_ax_cp(value, expected, f, ranges)
  loss = abs(f.Position(3:4) - [400 300]);
  tol = max(1e-9 + ranges .* (4 .* loss ./ [400 300]), ranges .* 0.01);
  v = value(1, 1:2);
  assert_istrue(all(abs(v - expected) <= tol));
end
%=============================================================================
