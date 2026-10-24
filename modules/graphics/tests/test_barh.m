%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
h = barh([3 5 2 7 4]);
assert_barh_object(h, 1);
asserts.isequal(char(h.Horizontal), 'on');
asserts.isequal(h.XData, [1 2 3 4 5]);
asserts.isequal(h.YData, [3 5 2 7 4]);
asserts.isequal(h.XEndPoints, [1 2 3 4 5]);
asserts.isequal(h.YEndPoints, [3 5 2 7 4]);
asserts.isapprox(h.EdgeColor, repmat(33 / 255, 1, 3), 1e-12);
ax = gca();
asserts.isapprox(ax.YLim, [-0.2 6.2], 1e-12);
asserts.isequal(ax.YTick, [1 2 3 4 5]);
asserts.isequal(char(ax.Box), 'on');
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = barh([1 2; 3 4; 5 6], 'grouped');
assert_barh_object(h, 2);
asserts.isapprox(h(1).XEndPoints, [0.857142857142857 1.857142857142857 2.857142857142857], 1e-12);
asserts.isapprox(h(2).XEndPoints, [1.142857142857143 2.142857142857143 3.142857142857143], 1e-12);
asserts.isequal(h(1).YEndPoints, [1 3 5]);
asserts.isequal(h(2).YEndPoints, [2 4 6]);
ax = gca();
asserts.isequal(ax.XLim, [0 6]);
asserts.isequal(ax.YTick, [1 2 3]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
y = [8 15 33; 30 35 40; 50 55 62];
h = barh(y, 'BaseValue', 25);
assert_barh_object(h, 3);
asserts.isequal(h(1).BaseValue, 25);
asserts.isequal(h(2).BaseValue, 25);
asserts.isequal(h(3).BaseValue, 25);
asserts.isequal(h(1).YEndPoints, [8 30 50]);
asserts.isequal(h(2).YEndPoints, [15 35 55]);
asserts.isequal(h(3).YEndPoints, [33 40 62]);
ax = gca();
asserts.isequal(ax.XLim, [0 70]);
asserts.isequal(ax.YTick, [1 2 3]);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 2 3];
vals = [2 3 6; 11 23 26];
h = barh(x, vals);
assert_barh_object(h, 2);
asserts.isequal(h(1).XData, x);
asserts.isequal(h(2).XData, x);
asserts.isequal(h(1).YData, vals(1, :));
asserts.isequal(h(2).YData, vals(2, :));
asserts.isapprox(h(1).XEndPoints, [0.857142857142857 1.857142857142857 2.857142857142857], 1e-12);
asserts.isapprox(h(2).XEndPoints, [1.142857142857143 2.142857142857143 3.142857142857143], 1e-12);
asserts.isequal(h(1).YEndPoints, vals(1, :));
asserts.isequal(h(2).YEndPoints, vals(2, :));
ax = gca();
asserts.isequal(ax.XLim, [0 30]);
asserts.isapprox(ax.YLim, [0.514285714285714 3.485714285714286], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1980 1990 2000];
y = [40 50 63 52; 42 55 50 48; 30 20 44 40];
barh(x, y);
[lgd, ~, plotHandles, textLabels] = legend({'Springfield', 'Fairview', 'Bristol', 'Jamesville'});
asserts.isequal(lgd.String, {'Springfield'; 'Fairview'; 'Bristol'; 'Jamesville'});
asserts.isequal(textLabels, {'Springfield', 'Fairview', 'Bristol', 'Jamesville'});
asserts.isequal(numel(plotHandles), 4);
asserts.istrue(all(isgraphics(plotHandles, 'bar')));
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
barh([1 2; 3 4; 5 6], 'grouped');
ax = gca();
asserts.isapprox(ax.DataAspectRatio, [3 1.485714285714286 1], 1e-12);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.788709677419355 0.788709677419355], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = barh([1 -2 3; -4 5 -6], 'stacked');
assert_barh_object(h, 3);
asserts.isequal(h(1).BarLayout, 'stacked');
asserts.isequal(h(1).XEndPoints, [1 2]);
asserts.isequal(h(2).XEndPoints, [1 2]);
asserts.isequal(h(3).XEndPoints, [1 2]);
asserts.isequal(h(1).YEndPoints, [1 -4]);
asserts.isequal(h(2).YEndPoints, [-2 5]);
asserts.isequal(h(3).YEndPoints, [4 -10]);
ax = gca();
asserts.isequal(ax.XLim, [-10 5]);
asserts.isequal(ax.YTick, [1 2]);
close(f);
f = figure('Visible', 'off');
h = barh([3 -2 5; -4 1 -3], 'stacked');
assert_barh_object(h, 3);
ax = gca();
asserts.isequal(ax.XLim, [-8 8]);
asserts.isequal(ax.XTick, -8:2:8);
asserts.isequal(ax.YTick, [1 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = barh([10 20 40], [2 4 6], 0.5, 'FaceColor', [0.3 0.2 0.1], ...
  'EdgeColor', [0.6 0.5 0.4]);
assert_barh_object(h, 1);
asserts.isequal(h.XData, [10 20 40]);
asserts.isequal(h.YData, [2 4 6]);
asserts.isapprox(h.BarWidth, 0.5, 1e-12);
asserts.isequal(h.FaceColor, [0.3 0.2 0.1]);
asserts.isequal(h.EdgeColor, [0.6 0.5 0.4]);
ax = gca();
asserts.isapprox(ax.YLim, [-0.5 50.5], 1e-12);
close(f);
%=============================================================================
caught = false;
try
  barh({1, 2});
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  barh([1 2], -1);
catch
  caught = true;
end
asserts.istrue(caught);
%=============================================================================
close all
%=============================================================================
function assert_barh_object(h, expectedCount)
  asserts.isequal(size(h), [1 expectedCount]);
  asserts.istrue(all(isgraphics(h, 'bar')));
  for k = 1:expectedCount
    asserts.isequal(h(k).Type, 'bar');
    asserts.isequal(char(h(k).Horizontal), 'on');
    asserts.isequal(char(h(k).ShowBaseLine), 'on');
  end
end
%=============================================================================
