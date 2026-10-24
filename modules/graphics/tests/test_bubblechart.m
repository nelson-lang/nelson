%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(1:3, [3 2 1], [20 40 60], 'r');
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(h.Type, 'bubblechart');
asserts.isequal(h.XData, [1 2 3]);
asserts.isequal(h.YData, [3 2 1]);
asserts.isequal(h.SizeData, [20 40 60]);
asserts.isequal(h.CData, [1 0 0]);
assert_bubblechart_properties(h);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = 1:20;
y = linspace(0.05, 0.95, 20);
sz = linspace(0.01, 0.99, 20);
h = bubblechart(x, y, sz);
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
asserts.isequal(h.SizeData, sz);
drawnow();
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(1:3, [3 2 1], [20 40 60]);
asserts.isequal(h.MarkerFaceColor, 'flat');
asserts.isequal(h.MarkerEdgeColor, 'flat');
asserts.isapprox(h.MarkerFaceAlpha, 0.6, 1e-12);
asserts.isapprox(h.MarkerEdgeAlpha, 1, 1e-12);
asserts.isapprox(h.LineWidth, 0.5, 1e-12);
asserts.isapprox(h.CData, gca().ColorOrder(1, :), 1e-12);
asserts.isequal(h.XJitter, 'none');
asserts.isequal(h.YJitter, 'none');
asserts.isequal(h.ZJitter, 'none');
asserts.isequal(h.XJitterDirection, 'both');
asserts.isequal(h.YJitterDirection, 'both');
asserts.isequal(h.ZJitterDirection, 'both');
asserts.isapprox(h.XJitterWidth, 0.9, 1e-12);
asserts.isapprox(h.YJitterWidth, 0.9, 1e-12);
asserts.isapprox(h.ZJitterWidth, 0.9, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'auto');
asserts.isequal(h.YJitterWidthMode, 'auto');
asserts.isequal(h.ZJitterWidthMode, 'auto');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
initialIndex = ax.ColorOrderIndex;
h = bubblechart(ax, 1:3, [3 2 1], [20 40 60], 'SeriesIndex', 'none');
asserts.isequal(h.SeriesIndex, 'none');
asserts.isequal(ax.ColorOrderIndex, initialIndex);
asserts.isapprox(h.CData, [33 33 33] / 255, 1e-12);
h.SeriesIndex = 2;
asserts.isequal(h.SeriesIndex, 2);
h.SeriesIndex = 'none';
asserts.isequal(h.SeriesIndex, 'none');
close(f);
%=============================================================================
f = figure('Visible', 'off');
rgb = [0.00 0.45 0.74; 0.85 0.33 0.10; 0.93 0.69 0.13];
h = bubblechart(1:3, [3 2 1], [20 40 60], rgb);
asserts.isequal(size(h.CData), [3 3]);
asserts.isapprox(h.CData, rgb, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = bubblechart(ax, 1:2, 3:4, [10 20], 'MarkerEdgeColor', 'k', ...
  'MarkerFaceAlpha', 0.25, 'MarkerEdgeAlpha', 0.75, 'LineWidth', 2, ...
  'DisplayName', 'series a', 'AlphaData', [0.1 0.9], 'XDataSource', 'x', ...
  'YDataSource', 'y', 'SizeDataSource', 's', 'CDataSource', 'c', ...
  'XJitterWidth', 0.2, 'XJitterWidthMode', 'manual');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.MarkerEdgeColor, [0 0 0]);
asserts.isapprox(h.MarkerFaceAlpha, 0.25, 1e-12);
asserts.isapprox(h.MarkerEdgeAlpha, 0.75, 1e-12);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.DisplayName, 'series a');
asserts.isequal(h.AlphaData, [0.1 0.9]);
asserts.isequal(h.XDataSource, 'x');
asserts.isequal(h.YDataSource, 'y');
asserts.isequal(h.SizeDataSource, 's');
asserts.isequal(h.CDataSource, 'c');
asserts.isapprox(h.XJitterWidth, 0.2, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(1:3, 2:4, 10);
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).XData, 1);
asserts.isequal(h(2).XData, 2);
asserts.isequal(h(3).XData, 3);
asserts.isequal(h(1).YData, 2);
asserts.isequal(h(2).YData, 3);
asserts.isequal(h(3).YData, 4);
asserts.isequal(h(1).SizeData, 10);
asserts.isapprox(h(1).CData, gca().ColorOrder(1, :), 1e-12);
asserts.isapprox(h(2).CData, gca().ColorOrder(2, :), 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(1:3, 2:4, [10 20; 30 40; 50 60]);
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).XData, [1 2 3]);
asserts.isequal(h(1).SizeData, [10 30 50]);
asserts.isequal(h(2).SizeData, [20 40 60]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 2 3; 4 5 6];
y = [2 3 4; 5 6 7];
sz = [10 20 30; 40 50 60];
h = bubblechart(x, y, sz, [0 1 0; 0 0 1; 1 0 0]);
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).XData, [1 4]);
asserts.isequal(h(2).XData, [2 5]);
asserts.isequal(h(3).XData, [3 6]);
asserts.isequal(h(1).SizeData, [10 40]);
asserts.isequal(h(2).SizeData, [20 50]);
asserts.isequal(h(3).CData, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(1:3, 2:4, [10 20 30], '#8000FF');
asserts.isapprox(h.CData, [0.5020 0 1.0000], 1e-4);
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table((1:4)', [4; 2; 6; 3], [20; 60; 30; 80], [1; 2; 3; 4], ...
  'VariableNames', {'x', 'y', 's', 'c'});
h = bubblechart(t, 'x', 'y', 's', 'c', 'DisplayName', 'from table');
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(h.YData, [4 2 6 3]);
asserts.isequal(h.SizeData, [20 60 30 80]);
asserts.isequal(h.CData(:).', [1 2 3 4]);
asserts.isequal(h.XVariable, 'x');
asserts.isequal(h.YVariable, 'y');
asserts.isequal(h.SizeVariable, 's');
asserts.isequal(h.ColorVariable, 'c');
asserts.isequal(h.DisplayName, 'from table');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart(t, [true false false false], [false true false false], ...
  [false false true false], [false false false true]);
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(h.YData, [4 2 6 3]);
asserts.isequal(h.SizeData, [20 60 30 80]);
asserts.isequal(h.CData(:).', [1 2 3 4]);
asserts.isequal(h.XVariable, 'x');
asserts.isequal(h.YVariable, 'y');
asserts.isequal(h.SizeVariable, 's');
asserts.isequal(h.ColorVariable, 'c');
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table((1:3)', [2; 4; 6], [3; 6; 9], [10; 20; 30], ...
  'VariableNames', {'x', 'y1', 'y2', 's'});
h = bubblechart(t, 'x', {'y1', 'y2'}, 's');
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).YData, [2 4 6]);
asserts.isequal(h(2).YData, [3 6 9]);
asserts.isequal(h(1).YVariable, 'y1');
asserts.isequal(h(2).YVariable, 'y2');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = bubblechart(ax, 1:3, 2:4, [10 100 1000]);
asserts.isequal(bubblelim(ax), [10 1000]);
asserts.isequal(bubblelim(ax, 'mode'), 'auto');
bubblelim(ax, [20 200]);
asserts.isequal(bubblelim(ax), [20 200]);
asserts.isequal(bubblelim(ax, 'mode'), 'manual');
bubblelim(ax, 'auto');
asserts.isequal(bubblelim(ax, 'mode'), 'auto');
asserts.isequal(bubblesize(ax), [3 50]);
bubblesize(ax, [5 30]);
asserts.isequal(bubblesize(ax), [5 30]);
asserts.isfalse(isprop(ax, 'BubbleLimits'));
asserts.isfalse(isprop(ax, 'BubbleSizeRange'));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
bubblechart(ax, 1:3, 2:4, [10 100 1000]);
bl = bubblelegend(ax, 'Town Population', 'Location', 'eastoutside');
asserts.istrue(isgraphics(bl, 'bubblelegend'));
asserts.isequal(bl.Type, 'bubblelegend');
asserts.isequal(bl.Location, 'eastoutside');
asserts.isequal(bl.NumBubbles, 3);
asserts.isequal(bl.Style, 'vertical');
asserts.isequal(bl.BubbleSizeOrder, 'descending');
asserts.istrue(isgraphics(bl.Title, 'text'));
asserts.isequal(bl.Title.String, 'Town Population');
asserts.istrue(isgraphics(bl.Layout, 'layoutoptions'));
assert_bubblelegend_properties(bl);
bl.NumBubbles = 2;
bl.Style = 'horizontal';
bl.Style = 'telescopic';
asserts.isequal(bl.Style, 'telescopic');
bl.Style = 'horizontal';
bl.BubbleSizeOrder = 'ascending';
bl.LimitLabels = {'low', 'high'};
bl.Box = 'off';
bl.Visible = 'off';
asserts.isequal(bl.NumBubbles, 2);
asserts.isequal(bl.Style, 'horizontal');
asserts.isequal(bl.BubbleSizeOrder, 'ascending');
asserts.isequal(bl.LimitLabels, {'low'; 'high'});
asserts.isequal(char(bl.Box), 'off');
asserts.isequal(char(bl.Visible), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
bubblechart(ax, 1:3, 2:4, [10 100 1000]);
bl = bubblelegend('Axes', ax, 'Location', 'eastoutside');
asserts.istrue(isgraphics(bl, 'bubblelegend'));
asserts.isequal(bl.Location, 'eastoutside');
asserts.isequal(bl.Title.String, '');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1200 760]);
t = tiledlayout(f, 1, 1);
ax = nexttile(t);
x = 1:20;
y1 = linspace(0.05, 0.95, 20);
y2 = linspace(0.95, 0.05, 20);
sz = linspace(20, 500, 20);
bubblechart(ax, x, y1, sz);
hold(ax, 'on');
bubblechart(ax, x, y2, sz);
hold(ax, 'off');
bl = bubblelegend(ax, 'Population');
lgd = legend(ax, {'Springfield', 'Fairview'});
bl.Layout.Tile = 'east';
lgd.Layout.Tile = 'east';
drawnow();
axpos = ax.Position;
blpos = bl.Position;
asserts.istrue(axpos(1) + axpos(3) < blpos(1));
f.Position = [100 100 720 460];
drawnow();
axpos = ax.Position;
blpos = bl.Position;
asserts.istrue(axpos(1) + axpos(3) < blpos(1));
asserts.istrue(blpos(1) + blpos(3) <= 1);
close(f);
%=============================================================================
% Auto axes limits must enclose every bubble, including the largest one near a
% frame edge. On a multi-series chart each series sizes its padding to the shared
% axis span, so the bubble radius (converted to data units from the rendered
% geometry) fits inside the padding and no bubble is clipped by the axes box.
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
x = [1 2 3; 4 5 6];
y = [2 4 3; 5 6 4];
sz = [20 40 60; 50 30 70];
bubblechart(x, y, sz);
drawnow();
ax = gca();
xr = [min(x(:)), max(x(:))];
yr = [min(y(:)), max(y(:))];
% The auto limits are strictly wider than the bare data range.
asserts.istrue(ax.XLim(1) < xr(1));
asserts.istrue(ax.XLim(2) > xr(2));
asserts.istrue(ax.YLim(1) < yr(1));
asserts.istrue(ax.YLim(2) > yr(2));
% The largest bubble is fully enclosed: its radius in data units (from the
% rendered axes pixel geometry) fits within the padding on every side.
axUnits = ax.Units;
ax.Units = 'pixels';
axPx = ax.Position;
ax.Units = axUnits;
dpi = get(groot, 'ScreenPixelsPerInch');
radiusPixels = max(bubblesize(ax)) * dpi / 72 / 2;
radDataX = radiusPixels * (ax.XLim(2) - ax.XLim(1)) / axPx(3);
radDataY = radiusPixels * (ax.YLim(2) - ax.YLim(1)) / axPx(4);
asserts.istrue(xr(2) + radDataX <= ax.XLim(2));
asserts.istrue(xr(1) - radDataX >= ax.XLim(1));
asserts.istrue(yr(2) + radDataY <= ax.YLim(2));
asserts.istrue(yr(1) - radDataY >= ax.YLim(1));
close(f);
%=============================================================================
asserts.checkerror('bubblechart(1, 2);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('bubblechart({1}, 2, 3);', getString(message('Nelson:bubblechart:numericXYExpected')));
asserts.checkerror('bubblechart(1:3, 2:4, []);', getString(message('Nelson:dimagree:sizeSMatchXY')));
asserts.checkerror('bubblechart(1:3, 2:4, [10 20 30], ''NoSuchProperty'', 1);', _('Unrecognized property NoSuchProperty for class BubbleChart.'));
% 'BubbleColor' is not a valid property; 2-D bubblechart rejects it with the same
% message as bubblechart3, so both stay consistent with the reference behavior.
asserts.checkerror('bubblechart(1:3, 2:4, [10 20 30], ''BubbleColor'', ''r'');', _('Unrecognized property BubbleColor for class BubbleChart.'));
asserts.checkerror('bubblelim([2 2]);', getString(message('Nelson:bubblelim:invalidValue')));
asserts.checkerror('bubblesize([30 5]);', getString(message('Nelson:bubblesize:invalidValue')));
%=============================================================================
function assert_bubblechart_properties(h)
  p = properties(h);
  expected = {'AlphaData'; 'AlphaDataMapping'; 'AlphaDataMode'; 'AlphaVariable';
    'BeingDeleted'; 'BusyAction'; 'CData'; 'CDataMode'; 'CDataSource'; 'ColorVariable';
    'DataTipTemplate'; 'DisplayName'; 'LineWidth'; 'MarkerEdgeAlpha';
    'MarkerEdgeColor'; 'MarkerFaceAlpha'; 'MarkerFaceColor'; 'SeriesIndex';
    'SizeData'; 'SizeDataMode'; 'SizeDataSource'; 'SizeVariable'; 'SourceTable';
    'Type'; 'XData'; 'XDataMode'; 'XDataSource'; 'XJitter'; 'XJitterDirection';
    'XJitterWidth'; 'XVariable'; 'YData'; 'YDataMode'; 'YDataSource'; 'YJitter';
    'YJitterDirection'; 'YJitterWidth'; 'YVariable'; 'ZData'; 'ZDataMode';
    'ZDataSource'; 'ZJitter'; 'ZJitterDirection'; 'ZJitterWidth'; 'ZVariable'};
  asserts.istrue(all(ismember(expected, p)));
  asserts.isfalse(any(strcmp(p, 'Marker')));
end
%=============================================================================
function assert_bubblelegend_properties(h)
  p = properties(h);
  expected = {'NumBubbles'; 'Style'; 'BubbleSizeOrder'; 'LimitLabels'; 'Location';
    'Title'; 'Box'; 'Color'; 'EdgeColor'; 'FontName'; 'FontSize'; 'FontAngle';
    'FontWeight'; 'Interpreter'; 'LineWidth'; 'Position'; 'TextColor'; 'Units';
    'Children'; 'Parent'; 'Visible'; 'HandleVisibility'; 'ButtonDownFcn';
    'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible'; 'CreateFcn';
    'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected'; 'SelectionHighlight';
    'HitTest'; 'PickableParts'; 'Layout'};
  asserts.istrue(all(ismember(expected, p)));
end
%=============================================================================
