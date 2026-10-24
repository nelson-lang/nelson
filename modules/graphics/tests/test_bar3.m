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
h = bar3([1 2; 3 4]);
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isequal(h(1).Parent, ax);
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(ax.YDir, 'reverse');
asserts.isequal(ax.SortMethod, 'depth');
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [1.8 3 1.8541019662496847], 1e-12);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isapprox(ax.XLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.YLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.ZLim, [0 4], 1e-12);
asserts.isapprox(ax.ZTick, 0:4, 1e-12);
assert_nan_matrix_isapprox(h(1).XData, detached_x1(), 1e-12);
assert_nan_matrix_isapprox(h(1).YData, detached_y1(), 1e-12);
assert_nan_matrix_isapprox(h(1).ZData, detached_z1(), 1e-12);
asserts.isequal(h(1).CData, ones(12, 4));
asserts.isequal(h(1).FaceLighting, 'flat');
asserts.isequal(h(1).BackFaceLighting, 'reverselit');
asserts.isequal(h(2).CData, 2 * ones(12, 4));
close(f);
%=============================================================================
f = figure('Visible', 'off');
Y = [1 2 3; 4 5 6];
h = bar3(Y);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 3.4], 1e-12);
asserts.isapprox(ax.YLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.ZLim, [0 6], 1e-12);
asserts.isapprox(ax.CameraViewAngle, 9.26296994456129, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [50 40 30 20 10];
h = bar3(z);
assert_surface_vector(h, 1);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isequal(ax.YDir, 'reverse');
asserts.isequal(ax.SortMethod, 'depth');
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [0.8 6 3.7082039324993694], 1e-12);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.CameraPositionMode, 'auto');
asserts.isapprox(ax.CLim, [1 2], 1e-12);
asserts.isequal(ax.CLimMode, 'manual');
asserts.isapprox(ax.YLim, [0.5 5.5], 1e-12);
asserts.isapprox(ax.ZLim, [0 50], 1e-12);
asserts.isapprox(ax.ZTick, 0:10:50, 1e-12);
asserts.isequal(size(h.XData), [30 4]);
asserts.isapprox(h.EdgeColor, repmat(33 / 255, 1, 3), 1e-12);
asserts.isequal(h.ZData(2:6:26, 2).', z);
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [1950 1960 1970 1980 1990];
y = [16 8 4 2 1];
h = bar3(z, y);
assert_surface_vector(h, 1);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isapprox(ax.YLim, [1945 1995], 1e-12);
asserts.isapprox(ax.ZLim, [0 16], 1e-12);
asserts.isapprox(ax.YTick, z, 1e-12);
asserts.isapprox(ax.ZTick, 0:2:16, 1e-12);
assert_plot_box_aspect(ax.PlotBoxAspectRatio, [0.8 6 3.7082039324993694], 1e-12);
asserts.isequal(size(h.XData), [30 4]);
asserts.isapprox(h.YData(2:6:26, 2).', z - 4, 1e-12);
asserts.isapprox(h.YData(3:6:27, 2).', z + 4, 1e-12);
asserts.isequal(h.ZData(2:6:26, 2).', y);
asserts.isequal(h.CData, ones(30, 4));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar3([1 2; 3 4], 'grouped');
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.8 1.2], 1e-12);
asserts.isapprox(ax.YLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.ZLim, [0 4], 1e-12);
assert_nan_matrix_isapprox(h(1).XData(1:4, :), grouped_x_rows(), 1e-6);
assert_nan_matrix_isapprox(h(1).YData(1:4, :), grouped_y_col1_row1(), 1e-6);
assert_nan_matrix_isapprox(h(2).YData(7:10, :), grouped_y_col2_row2(), 1e-6);
close(f);
%=============================================================================
f = figure('Visible', 'off');
y = [1 2; 3 4; 5 6];
h = bar3(y, 'grouped');
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.8 1.2], 1e-12);
asserts.isapprox(ax.YLim, [0.5 3.5], 1e-12);
asserts.isapprox(ax.ZLim, [0 6], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar3([1 -2; -3 4], 'stacked');
assert_surface_vector(h, 2);
ax = ancestor(h(1), 'axes');
asserts.isapprox(ax.XLim, [0.6 1.4], 1e-12);
asserts.isapprox(ax.YLim, [0.6 2.4], 1e-12);
asserts.isapprox(ax.ZLim, [-3 1], 1e-12);
assert_nan_matrix_isapprox(h(2).ZData, stacked_z2(), 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = bar3(ax, [1 0; -2 3], 0.5, 'FaceColor', [0.2 0.4 0.6], 'FaceAlpha', 0.75);
assert_surface_vector(h, 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).FaceColor, [0.2 0.4 0.6]);
asserts.isapprox(h(1).FaceAlpha, 0.75, 1e-12);
close(f);
%=============================================================================
caught = false;
try
  bar3({1, 2});
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  bar3([1 2], 0);
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  bar3([1 2], 'Unknown', 1);
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
function x = grouped_x_rows()
  x = [NaN 0.885714285714286 1.114285714285714 NaN; ...
    0.885714285714286 0.885714285714286 1.114285714285714 1.114285714285714; ...
    0.885714285714286 0.885714285714286 1.114285714285714 1.114285714285714; ...
    NaN 0.885714285714286 1.114285714285714 NaN];
end
%=============================================================================
function y = grouped_y_col1_row1()
  y = [NaN 0.742857142857143 0.742857142857143 NaN; ...
    0.742857142857143 0.742857142857143 0.742857142857143 0.742857142857143; ...
    0.971428571428571 0.971428571428571 0.971428571428571 0.971428571428571; ...
    NaN 0.971428571428571 0.971428571428571 NaN];
end
%=============================================================================
function y = grouped_y_col2_row2()
  y = [NaN 2.028571428571429 2.028571428571429 NaN; ...
    2.028571428571429 2.028571428571429 2.028571428571429 2.028571428571429; ...
    2.257142857142857 2.257142857142857 2.257142857142857 2.257142857142857; ...
    NaN 2.257142857142857 2.257142857142857 NaN];
end
%=============================================================================
function z = stacked_z2()
  z = [NaN 1 1 NaN; 1 -1 -1 1; 1 -1 -1 1; NaN 1 1 NaN; ...
    NaN 1 1 NaN; NaN NaN NaN NaN; NaN -3 -3 NaN; -3 1 1 -3; ...
    -3 1 1 -3; NaN -3 -3 NaN; NaN -3 -3 NaN; NaN NaN NaN NaN];
end
%=============================================================================
