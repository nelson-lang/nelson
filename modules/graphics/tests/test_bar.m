%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
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
h = bar([1 2; 3 4; 5 6], 'grouped');
assert_bar_object(h, 2);
assert_bar_public_properties(h(1));
asserts.isequal(h(1).BarLayout, 'grouped');
asserts.isequal(char(h(1).ShowBaseLine), 'on');
asserts.isapprox(h(1).BarWidth, 0.8, 1e-12);
asserts.isapprox(h(1).GroupWidth, 2 / 3.5, 1e-12);
asserts.isapprox(h(1).EdgeColor, repmat(33 / 255, 1, 3), 1e-12);
asserts.isequal(char(h(1).Horizontal), 'off');
asserts.isequal(h(1).XData, [1 2 3]);
asserts.isequal(h(1).YData, [1 3 5]);
asserts.isapprox(h(1).XEndPoints, [0.857142857142857 1.857142857142857 2.857142857142857], 1e-12);
asserts.isapprox(h(2).XEndPoints, [1.142857142857143 2.142857142857143 3.142857142857143], 1e-12);
asserts.isequal(h(1).YEndPoints, [1 3 5]);
asserts.isequal(h(2).YEndPoints, [2 4 6]);
ax = gca();
asserts.isequal(ax.XTick, [1 2 3]);
asserts.isequal(ax.YLim, [0 6]);
asserts.isequal(char(ax.Box), 'on');
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 420 260]);
ax = axes('Parent', f, 'Position', [0.08 0.08 0.88 0.86]);
h = bar(ax, 1, [1 1], 'grouped');
drawnow();
frame = getframe(f);
img = frame.cdata;
bluePixels = img(:, :, 1) < 80 & img(:, :, 2) > 60 & img(:, :, 2) < 150 & img(:, :, 3) > 120;
orangePixels = img(:, :, 1) > 180 & img(:, :, 2) > 60 & img(:, :, 2) < 150 & img(:, :, 3) < 80;
asserts.istrue(nnz(bluePixels) > 100);
asserts.istrue(nnz(orangePixels) > 100);
[dummy, blueCols] = find(bluePixels);
[dummy, orangeCols] = find(orangePixels);
asserts.istrue(min(orangeCols) - max(blueCols) > 8);
asserts.isequal(numel(h), 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 2 3];
vals = [2 3 6; 11 23 26];
h = bar(x, vals);
assert_bar_object(h, 2);
asserts.isequal(h(1).XData, x);
asserts.isequal(h(2).XData, x);
asserts.isequal(h(1).YData, vals(1, :));
asserts.isequal(h(2).YData, vals(2, :));
asserts.isapprox(h(1).XEndPoints, [0.857142857142857 1.857142857142857 2.857142857142857], 1e-12);
asserts.isapprox(h(2).XEndPoints, [1.142857142857143 2.142857142857143 3.142857142857143], 1e-12);
asserts.isequal(h(1).YEndPoints, vals(1, :));
asserts.isequal(h(2).YEndPoints, vals(2, :));
ax = gca();
asserts.isapprox(ax.XLim, [0.514285714285714 3.485714285714286], 1e-12);
asserts.isequal(ax.YLim, [0 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
bar([1 2; 3 4; 5 6], 'grouped');
ax = gca();
asserts.isapprox(ax.DataAspectRatio, [1.485714285714286 3 1], 1e-12);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.788709677419355 0.788709677419355], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar([1 -2 3; -4 5 -6], 'stacked');
assert_bar_object(h, 3);
asserts.isequal(h(1).BarLayout, 'stacked');
asserts.isequal(h(1).XEndPoints, [1 2]);
asserts.isequal(h(2).XEndPoints, [1 2]);
asserts.isequal(h(3).XEndPoints, [1 2]);
asserts.isequal(h(1).YEndPoints, [1 -4]);
asserts.isequal(h(2).YEndPoints, [-2 5]);
asserts.isequal(h(3).YEndPoints, [4 -10]);
ax = gca();
asserts.isequal(ax.XTick, [1 2]);
asserts.isequal(ax.YLim, [-10 5]);
asserts.isequal(ax.YTick, [-10 -5 0 5]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = 2020;
y = [30 50 23];
h = bar(x, y, "stacked");
assert_bar_object(h, 3);
asserts.isequal(h(1).BarLayout, 'stacked');
asserts.isequal(char(h(1).Horizontal), 'off');
asserts.isequal(h(1).XData, 2020);
asserts.isequal(h(2).XData, 2020);
asserts.isequal(h(3).XData, 2020);
asserts.isequal(h(1).YData, 30);
asserts.isequal(h(2).YData, 50);
asserts.isequal(h(3).YData, 23);
asserts.isequal(h(1).XEndPoints, 2020);
asserts.isequal(h(2).XEndPoints, 2020);
asserts.isequal(h(3).XEndPoints, 2020);
asserts.isequal(h(1).YEndPoints, 30);
asserts.isequal(h(2).YEndPoints, 80);
asserts.isequal(h(3).YEndPoints, 103);
ax = gca();
asserts.isapprox(ax.XLim, [2018.8 2021.2], 1e-12);
asserts.isequal(ax.YLim, [0 120]);
asserts.isequal(ax.XTick, 2020);
asserts.isequal(ax.YTick, 0:20:120);
close(f);
%=============================================================================
% Automatic value-axis limits follow the reference nice-number grid: a stacked
% total that lands just above a round decade snaps to the next tick (70), not to
% an extra one (80).
f = figure('Visible', 'off');
h = bar(magic(5), 'stacked');
assert_bar_object(h, 5);
ax = gca();
asserts.isequal(ax.YLim, [0 70]);
asserts.isequal(ax.YTick, 0:10:70);
close(f);
%=============================================================================
% A single value already on a {1,2,5} grid keeps a tight upper limit.
f = figure('Visible', 'off');
bar(15);
ax = gca();
asserts.isequal(ax.YLim, [0 15]);
asserts.isequal(ax.YTick, 0:5:15);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar([10 20 40], [2 4 6], 0.5, 'FaceColor', [0.1 0.2 0.3], ...
  'EdgeColor', [0.4 0.5 0.6], 'LineWidth', 1.5, 'DisplayName', 'values');
assert_bar_object(h, 1);
asserts.isequal(h.XData, [10 20 40]);
asserts.isequal(h.YData, [2 4 6]);
asserts.isapprox(h.BarWidth, 0.5, 1e-12);
asserts.isequal(h.FaceColor, [0.1 0.2 0.3]);
asserts.isequal(h.EdgeColor, [0.4 0.5 0.6]);
asserts.isapprox(h.LineWidth, 1.5, 1e-12);
asserts.isequal(h.DisplayName, 'values');
ax = gca();
asserts.isapprox(ax.XLim, [-0.5 50.5], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar({'Spring', 'Summer', 'Autumn', 'Winter'}, [1 2 3 4], 'r');
assert_bar_object(h, 1);
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(h.YData, [1 2 3 4]);
asserts.isequal(h.FaceColor, [1 0 0]);
ax = gca();
asserts.isequal(ax.XTickLabel, {'Spring'; 'Summer'; 'Autumn'; 'Winter'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
X = categorical({'Small', 'Medium', 'Large', 'Extra Large'});
X = reordercats(X, {'Small', 'Medium', 'Large', 'Extra Large'});
Y = [10 21 33 52];
h = bar(X, Y);
assert_bar_object(h, 1);
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(h.YData, Y);
asserts.isequal(h.XEndPoints, [1 2 3 4]);
asserts.isequal(h.YEndPoints, Y);
ax = gca();
asserts.isequal(class(ax.XLim), 'categorical');
asserts.isequal(ax.YLim, [0 60]);
asserts.isequal(class(ax.XTick), 'categorical');
asserts.isequal(ax.XTickLabel, {'Small'; 'Medium'; 'Large'; 'Extra Large'});
asserts.isequal(ax.YTick, 0:10:60);
close(f);
%=============================================================================
f = figure('Visible', 'off');
Month = ["April"; "May"; "June"; "July"; "August"];
Sales = [2000; 3000; 4000; 5000; 6000];
Revenue = [1500; 1800; 2000; 3000; 4000];
tbl = table(Month, Sales, Revenue);
h = bar(tbl, "Month", "Sales");
assert_bar_object(h, 1);
asserts.isequal(h.XData, 1:5);
asserts.isequal(h.YData, Sales.');
asserts.isequal(h.XEndPoints, 1:5);
asserts.isequal(h.YEndPoints, Sales.');
ax = gca();
asserts.isequal(class(ax.XLim), 'categorical');
asserts.isequal(ax.YLim, [0 6000]);
asserts.isequal(class(ax.XTick), 'categorical');
asserts.isequal(ax.XTickLabel, {'April'; 'May'; 'June'; 'July'; 'August'});
asserts.isequal(ax.YTick, 0:1000:6000);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar(tbl, "Month", ["Sales", "Revenue"]);
assert_bar_object(h, 2);
asserts.isequal(h(1).DisplayName, 'Sales');
asserts.isequal(h(2).DisplayName, 'Revenue');
asserts.isequal(h(1).XData, 1:5);
asserts.isequal(h(2).XData, 1:5);
asserts.isequal(h(1).YData, Sales.');
asserts.isequal(h(2).YData, Revenue.');
asserts.isapprox(h(1).XEndPoints, [0.857142857142857 1.857142857142857 2.857142857142857 3.857142857142857 4.857142857142857], 1e-12);
asserts.isapprox(h(2).XEndPoints, [1.142857142857143 2.142857142857143 3.142857142857143 4.142857142857143 5.142857142857143], 1e-12);
asserts.isequal(h(1).YEndPoints, Sales.');
asserts.isequal(h(2).YEndPoints, Revenue.');
lgd = legend(Location= "northwest");
asserts.isequal(lgd.Location, 'northwest');
ax = gca();
asserts.isequal(class(ax.XLim), 'categorical');
asserts.isequal(ax.YLim, [0 6000]);
asserts.isequal(class(ax.XTick), 'categorical');
asserts.isequal(ax.XTickLabel, {'April'; 'May'; 'June'; 'July'; 'August'});
asserts.isequal(ax.YTick, 0:1000:6000);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar(tbl, 1, [false true true]);
assert_bar_object(h, 2);
asserts.isequal(h(1).DisplayName, 'Sales');
asserts.isequal(h(2).DisplayName, 'Revenue');
asserts.isequal(h(1).YData, Sales.');
asserts.isequal(h(2).YData, Revenue.');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar([3 5 2 7 4]);
assert_bar_object(h, 1);
ax = gca();
asserts.isapprox(ax.XLim, [-0.2 6.2], 1e-12);
asserts.isequal(ax.XTick, [1 2 3 4 5]);
close(f);
%=============================================================================
% Documented bar(Y, width) form: a row vector Y with a scalar width in (0, 1].
% The scalar is the bar width, not a second data series, and it must render
% without terminating the process (regression for the reported hard crash).
f = figure('Visible', 'off');
y = [91 75 123.5 105 150 131 203 179 249 226 281.5];
h = bar(y, 0.5);
assert_bar_object(h, 1);
asserts.isapprox(h.BarWidth, 0.5, 1e-12);
asserts.isequal(h.XData, 1:11);
asserts.isequal(h.YData, y);
asserts.isequal(h.YEndPoints, y);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
% The same width also applies through the bar(X, Y, width) form.
f = figure('Visible', 'off');
x = 1900:10:2000;
h = bar(x, y, 0.5);
assert_bar_object(h, 1);
asserts.isapprox(h.BarWidth, 0.5, 1e-12);
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
drawnow();
close(f);
%=============================================================================
caught = false;
try
  bar({1, 2});
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  bar([1 2], 0);
catch
  caught = true;
end
asserts.istrue(caught);
%=============================================================================
close all
%=============================================================================
function assert_bar_object(h, expectedCount)
  asserts.isequal(size(h), [1 expectedCount]);
  asserts.istrue(all(isgraphics(h, 'bar')));
  for k = 1:expectedCount
    asserts.isequal(h(k).Type, 'bar');
  end
end
%=============================================================================
function assert_bar_public_properties(h)
  required = {'BarLayout'; 'BarWidth'; 'GroupWidth'; 'GroupWidthMode'; 'BaseLine'; ...
    'BaseValue'; 'ShowBaseLine'; 'Horizontal'; 'XData'; 'YData'; 'XEndPoints'; ...
    'YEndPoints'; 'FaceColor'; 'FaceColorMode'; 'EdgeColor'; 'EdgeAlpha'; 'FaceAlpha'; ...
    'CData'; 'CDataMode'; 'Labels'; 'LabelLocation'; 'LabelLocationMode'; ...
    'LabelColor'; 'LabelColorMode'; 'FontName'; 'FontSize'; 'FontWeight'; ...
    'FontAngle'; 'Interpreter'; 'XDataSource'; 'YDataSource'; 'DisplayName'; ...
    'DataTipTemplate'; 'SeriesIndex'};
  props = properties(h);
  asserts.istrue(all(ismember(required, props)));
end
%=============================================================================
