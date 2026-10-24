%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
h = bar3h([1 2; 3 4]);
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isequal(h(1).Parent, ax);
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(ax.YDir, 'reverse');
asserts.isequal(ax.SortMethod, 'depth');
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [1.8 1.8541019662496847 3], 1e-12);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isapprox(ax.XLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.YLim, [0 5], 1e-12);
asserts.isapprox(ax.ZLim, [0.6 2.4], 1e-12);
assert_nan_matrix_isapprox(h(1).XData, detached_x1(), 1e-12);
assert_nan_matrix_isapprox(h(1).YData, detached_z1(), 1e-12);
assert_nan_matrix_isapprox(h(1).ZData, detached_y1(), 1e-12);
asserts.isequal(h(1).FaceLighting, 'flat');
asserts.isequal(h(1).BackFaceLighting, 'reverselit');
close(f);
%=============================================================================
f = figure('Visible', 'off');
y = [1 4 7; 2 5 8; 3 6 9; 4 7 10];
h = bar3h(y);
assert_surface_vector(h, 3);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.CLim, [1 3], 1e-12);
asserts.isequal(h(1).CData, ones(24, 4));
asserts.isequal(h(2).CData, 2 * ones(24, 4));
asserts.isequal(h(3).CData, 3 * ones(24, 4));
asserts.isequal(h(1).FaceColor, 'flat');
asserts.isequal(h(2).FaceColor, 'flat');
asserts.isequal(h(3).FaceColor, 'flat');
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [50 40 30 20 10];
h = bar3h(z);
assert_surface_vector(h, 1);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isequal(ax.YDir, 'reverse');
asserts.isequal(ax.SortMethod, 'depth');
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [0.8 3.7082039324993694 6], 1e-12);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.CameraPositionMode, 'auto');
asserts.isapprox(ax.CLim, [1 2], 1e-12);
asserts.isequal(ax.CLimMode, 'manual');
asserts.isapprox(ax.YLim, [0 50], 1e-12);
asserts.isapprox(ax.ZLim, [0.5 5.5], 1e-12);
asserts.istrue(isempty(ax.XTick));
asserts.isapprox(ax.YTick, 0:10:50, 1e-12);
asserts.isapprox(ax.ZTick, 1:5, 1e-12);
asserts.isequal(size(h.XData), [30 4]);
asserts.isapprox(h.EdgeColor, repmat(33 / 255, 1, 3), 1e-12);
asserts.isequal(h.YData(2:6:26, 2).', z);
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [1950 1960 1970 1980 1990];
y = [16 8 4 2 1];
h = bar3h(z, y);
assert_surface_vector(h, 1);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isapprox(ax.YLim, [0 20], 1e-12);
asserts.isapprox(ax.ZLim, [1945 1995], 1e-12);
asserts.isapprox(ax.YTick, 0:5:20, 1e-12);
asserts.isapprox(ax.ZTick, z, 1e-12);
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [0.8 3.7082039324993694 6], 1e-12);
asserts.isequal(size(h.XData), [30 4]);
asserts.isequal(h.YData(2:6:26, 2).', y);
asserts.isapprox(h.ZData(2:6:26, 2).', z - 4, 1e-12);
asserts.isapprox(h.ZData(3:6:27, 2).', z + 4, 1e-12);
asserts.isequal(h.CData, ones(30, 4));
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [1 2 3 4];
y = [1 5 9; 2 6 10; 3 7 11; 4 8 12];
h = bar3h(z, y);
assert_surface_vector(h, 3);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 3.4], 1e-12);
asserts.isapprox(ax.YLim, [0 15], 1e-12);
asserts.isapprox(ax.ZLim, [0.5 4.5], 1e-12);
asserts.isapprox(ax.XTick, 1:3, 1e-12);
asserts.isapprox(ax.YTick, 0:5:15, 1e-12);
asserts.isapprox(ax.ZTick, z, 1e-12);
asserts.isequal(h(1).CData, ones(24, 4));
asserts.isequal(h(2).CData, 2 * ones(24, 4));
asserts.isequal(h(3).CData, 3 * ones(24, 4));
asserts.isapprox(h(1).YData(2:6:20, 2).', y(:, 1).', 1e-12);
asserts.isapprox(h(2).YData(2:6:20, 2).', y(:, 2).', 1e-12);
asserts.isapprox(h(3).YData(2:6:20, 2).', y(:, 3).', 1e-12);
asserts.isapprox(h(1).ZData(2:6:20, 2).', z - 0.4, 1e-12);
asserts.isapprox(h(1).ZData(3:6:21, 2).', z + 0.4, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = 0:pi / 16:pi;
y = [sin(z') / 4, sin(z') / 2, sin(z')];
h = bar3h(z, y, 1, "r");
assert_surface_vector(h, 3);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.5 3.5], 1e-12);
asserts.isapprox(ax.YLim, [0 1], 1e-12);
asserts.isapprox(ax.ZLim, [-0.5 3.5], 1e-12);
asserts.isapprox(ax.XTick, 1:3, 1e-12);
asserts.isapprox(ax.YTick, 0:0.2:1, 1e-12);
asserts.isapprox(ax.ZTick, -0.5:0.5:3.5, 1e-12);
asserts.isequal(h(1).FaceColor, [1 0 0]);
asserts.isequal(h(2).FaceColor, [1 0 0]);
asserts.isequal(h(3).FaceColor, [1 0 0]);
asserts.isapprox(h(1).EdgeColor, repmat(33 / 255, 1, 3), 1e-12);
asserts.isapprox(h(1).ZData(2:6:98, 2).', z - pi / 32, 1e-12);
asserts.isapprox(h(1).ZData(3:6:99, 2).', z + pi / 32, 1e-12);
asserts.isapprox(h(1).YData(2:6:98, 2).', sin(z) / 4, 1e-12);
asserts.isapprox(h(2).YData(2:6:98, 2).', sin(z) / 2, 1e-12);
asserts.isapprox(h(3).YData(2:6:98, 2).', sin(z), 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar3h([1 -2; -3 4], 'stacked');
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isapprox(ax.YLim, [-3 1], 1e-12);
asserts.isapprox(ax.ZLim, [0.6 2.4], 1e-12);
assert_nan_matrix_isapprox(h(2).YData, stacked_y2(), 1e-12);
assert_nan_matrix_isapprox(h(2).ZData, detached_y1(), 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = bar3h(ax, [1 0; -2 3], 'EdgeColor', [0.1 0.2 0.3]);
assert_surface_vector(h, 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).EdgeColor, [0.1 0.2 0.3]);
close(f);
%=============================================================================
caught = false;
try
  bar3h({1, 2});
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  bar3h([1 2], 'Unknown', 1);
catch
  caught = true;
end
asserts.istrue(caught);
%=============================================================================
close all
%=============================================================================
function assert_surface_vector(h, expectedCount)
  asserts.isequal(size(h), [1 expectedCount]);
  asserts.istrue(all(isgraphics(h, 'surface')));
  for k = 1:expectedCount
    asserts.isequal(h(k).Type, 'surface');
  end
end
%=============================================================================
function assert_plot_box_aspect(actual, expected, tol)
  actual = actual / actual(1);
  expected = expected / expected(1);
  asserts.isapprox(actual, expected, tol);
end
%=============================================================================
function assert_nan_matrix_isapprox(actual, expected, tol)
  asserts.isequal(size(actual), size(expected));
  asserts.istrue(all(isnan(actual(:)) == isnan(expected(:))));
  mask = ~isnan(expected);
  if any(mask(:))
    asserts.isapprox(actual(mask), expected(mask), tol);
  end
end
%=============================================================================
function x = detached_x1()
  x = [NaN 0.6 1.4 NaN; 0.6 0.6 1.4 1.4; 0.6 0.6 1.4 1.4; ...
    NaN 0.6 1.4 NaN; NaN 0.6 1.4 NaN; NaN NaN NaN NaN; ...
    NaN 0.6 1.4 NaN; 0.6 0.6 1.4 1.4; 0.6 0.6 1.4 1.4; ...
    NaN 0.6 1.4 NaN; NaN 0.6 1.4 NaN; NaN NaN NaN NaN];
end
%=============================================================================
function y = detached_y1()
  y = [NaN 0.6 0.6 NaN; 0.6 0.6 0.6 0.6; 1.4 1.4 1.4 1.4; ...
    NaN 1.4 1.4 NaN; NaN 0.6 0.6 NaN; NaN NaN NaN NaN; ...
    NaN 1.6 1.6 NaN; 1.6 1.6 1.6 1.6; 2.4 2.4 2.4 2.4; ...
    NaN 2.4 2.4 NaN; NaN 1.6 1.6 NaN; NaN NaN NaN NaN];
end
%=============================================================================
function z = detached_z1()
  z = [NaN 0 0 NaN; 0 1 1 0; 0 1 1 0; NaN 0 0 NaN; ...
    NaN 0 0 NaN; NaN NaN NaN NaN; NaN 0 0 NaN; 0 3 3 0; ...
    0 3 3 0; NaN 0 0 NaN; NaN 0 0 NaN; NaN NaN NaN NaN];
end
%=============================================================================
function y = stacked_y2()
  y = [NaN 1 1 NaN; 1 -1 -1 1; 1 -1 -1 1; NaN 1 1 NaN; ...
    NaN 1 1 NaN; NaN NaN NaN NaN; NaN -3 -3 NaN; -3 1 1 -3; ...
    -3 1 1 -3; NaN -3 -3 NaN; NaN -3 -3 NaN; NaN NaN NaN NaN];
end
%=============================================================================
