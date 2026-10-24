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
f = figure('Visible', 'off');
r = raincloudplot([1 2 2 3 4]);
asserts.istrue(isgraphics(r, 'raincloudplot'));
asserts.isequal(r.Type, 'raincloudplot');
asserts.isequal(class(r), 'nelson.graphics.chart.primitive.RainCloudPlot');
asserts.isequal(size(r), [1 1]);
asserts.isequal(r.XData, ones(5, 1));
asserts.isequal(r.YData, [1; 2; 2; 3; 4]);
asserts.isequal(r.XDataMode, 'auto');
asserts.isequal(r.YDataMode, 'manual');
asserts.isequal(r.Orientation, 'horizontal');
asserts.isapprox(r.DensityWidth, 0.9, 1e-12);
asserts.isapprox(r.FaceAlpha, 0.2, 1e-12);
asserts.isapprox(r.LineWidth, 0.5, 1e-12);
asserts.isequal(r.Marker, 'o');
asserts.isapprox(r.MarkerSize, 36, 1e-12);
asserts.isequal(r.SeriesIndex, 1);
ax = gca();
colors = ax.ColorOrder;
asserts.isapprox(r.FaceColor, colors(1, :), 1e-12);
asserts.isapprox(r.EdgeColor, colors(1, :), 1e-12);
asserts.isapprox(r.MarkerFaceColor, colors(1, :), 1e-12);
asserts.isapprox(r.MarkerEdgeColor, colors(1, :), 1e-12);
asserts.isequal(r.FaceColorMode, 'auto');
asserts.isequal(r.EdgeColorMode, 'auto');
asserts.isequal(r.MarkerFaceColorMode, 'auto');
asserts.isequal(r.MarkerEdgeColorMode, 'auto');
asserts.isequal(r.DisplayName, '');
asserts.istrue(isempty(r.Children));
drawnow();
asserts.isequal(numel(ax.Children), 1);
asserts.isequal(ax.Children(1), r);
asserts.isequal(ax.YTick, 1);
close(f);
%=============================================================================
% Color modes: edge and marker colors follow the face color while in auto mode.
f = figure('Visible', 'off');
r = raincloudplot([1 2 2 3 4]);
r.FaceColor = 'g';
asserts.isequal(r.FaceColorMode, 'manual');
asserts.isapprox(r.FaceColor, [0 1 0], 1e-12);
drawnow();
asserts.isapprox(r.EdgeColor, [0 1 0], 1e-12);
asserts.isapprox(r.MarkerFaceColor, [0 1 0], 1e-12);
r.EdgeColor = [1 0 0];
drawnow();
asserts.isequal(r.EdgeColorMode, 'manual');
asserts.isapprox(r.MarkerEdgeColor, [1 0 0], 1e-12);
r.MarkerFaceColor = 'none';
drawnow();
asserts.isequal(r.MarkerFaceColorMode, 'manual');
asserts.isequal(r.MarkerFaceColor, 'none');
close(f);
%=============================================================================
% One object per matrix column, colored in creation order.
f = figure('Visible', 'off');
r = raincloudplot([1 2 3; 4 5 6; 7 8 10]);
asserts.isequal(size(r), [3 1]);
asserts.isequal(r(1).XData, [1; 1; 1]);
asserts.isequal(r(2).XData, [2; 2; 2]);
asserts.isequal(r(3).XData, [3; 3; 3]);
asserts.isequal(r(3).YData, [3; 6; 10]);
asserts.isequal([r.SeriesIndex], [1 2 3]);
colors = gca().ColorOrder;
asserts.isapprox(r(2).FaceColor, colors(2, :), 1e-12);
asserts.isequal(gca().YTick, [1 2 3]);
close(f);
%=============================================================================
% hold on: a new plot continues the series index.
f = figure('Visible', 'off');
r1 = raincloudplot([1 2 3 4]);
hold on
r2 = raincloudplot([2 3 4 5]);
asserts.isequal(r1.SeriesIndex, 1);
asserts.isequal(r2.SeriesIndex, 2);
asserts.isequal(numel(gca().Children), 2);
legend('first', 'second');
drawnow();
close(f);
%=============================================================================
% Positional grouping data: numeric and categorical.
f = figure('Visible', 'off');
r = raincloudplot([1 1 2 2 2], [10 11 20 21 22]);
asserts.isequal(size(r), [1 1]);
asserts.isequal(r.XData, [1; 1; 2; 2; 2]);
asserts.isequal(r.XDataMode, 'manual');
asserts.isequal(gca().YTick, [1 2]);
close(f);
f = figure('Visible', 'off');
x = categorical(repelem(["group1"; "group2"; "group3"], [2, 5, 3]));
r = raincloudplot(x, (1:10)');
asserts.isequal(size(r), [1 1]);
asserts.isequal(r.XData, [1; 1; 2; 2; 2; 2; 2; 3; 3; 3]);
ax = gca();
asserts.isequal(ax.YTick, [1 2 3]);
asserts.isequal(ax.YTickLabel, {'group1'; 'group2'; 'group3'});
close(f);
%=============================================================================
% Table input: one object per variable, shared category positions.
X1 = categorical(repelem(["group1"; "group2"], [6, 4]));
X3 = categorical(repelem(["group3"; "group4"], [3, 7]));
Y1 = (1:10)';
Y2 = (11:20)';
tbl = table(X1, X3, Y1, Y2);
f = figure('Visible', 'off');
r = raincloudplot(tbl, 'Y1');
asserts.isequal(size(r), [1 1]);
asserts.isequal(r.YData, Y1);
asserts.isequal(r.YVariable, 'Y1');
asserts.isequal(r.XDataMode, 'auto');
asserts.isequal(r.YDataMode, 'auto');
asserts.istrue(istable(r.SourceTable));
close(f);
f = figure('Visible', 'off');
r = raincloudplot(tbl, ["Y1", "Y2"]);
asserts.isequal(size(r), [2 1]);
asserts.isequal(r(2).YData, Y2);
asserts.isequal(r(2).XData, ones(10, 1));
close(f);
f = figure('Visible', 'off');
r = raincloudplot(tbl, ["X1", "X3"], ["Y1", "Y2"]);
asserts.isequal(size(r), [2 1]);
asserts.isequal(unique(r(1).XData)', [1 2]);
asserts.isequal(unique(r(2).XData)', [3 4]);
asserts.isequal(r(2).XVariable, 'X3');
asserts.isequal(r(2).YVariable, 'Y2');
asserts.isequal(gca().YTickLabel, {'group1'; 'group2'; 'group3'; 'group4'});
close(f);
f = figure('Visible', 'off');
r = raincloudplot(tbl, ["X1", "X3"], "Y1");
asserts.isequal(size(r), [2 1]);
asserts.isequal(r(1).YData, r(2).YData);
close(f);
%=============================================================================
% Target axes, name-value arguments and vertical orientation.
f = figure('Visible', 'off');
ax1 = subplot(1, 2, 1);
ax2 = subplot(1, 2, 2);
r = raincloudplot(ax2, [1 1 2 2], [5 6 7 8], 'Orientation', 'vertical', 'DensityWidth', 0.5, 'FaceAlpha', 0.4, 'Marker', 's', 'MarkerSize', 20);
asserts.isequal(r.Parent, ax2);
asserts.isequal(numel(ax1.Children), 0);
asserts.isequal(r.Orientation, 'vertical');
asserts.isapprox(r.DensityWidth, 0.5, 1e-12);
asserts.isapprox(r.FaceAlpha, 0.4, 1e-12);
asserts.isequal(r.Marker, 'square');
asserts.isapprox(r.MarkerSize, 20, 1e-12);
asserts.isequal(ax2.XTick, [1 2]);
drawnow();
close(f);
%=============================================================================
% Web renderer: one cloud polygon per group and one marker per sample. The
% markers are one "points" batch whose count is the number of samples.
f = figure('Visible', 'off');
r = raincloudplot([1 1 1 2 2 2], [1 2 2 3 4 5]);
drawnow();
withRain = __web_display_list__(f);
asserts.isequal(count(withRain, '"kind":"points"'), 1);
markerCount = regexp(withRain, '"count":(\d+)[^{}]*"kind":"points"', 'tokens', 'once');
asserts.isequal(str2double(markerCount{1}), 6);
withRainPolygons = count(withRain, '"kind":"polygon"');
r.Marker = 'none';
drawnow();
withoutRain = __web_display_list__(f);
asserts.isequal(count(withoutRain, '"kind":"points"'), 0);
withoutRainPolygons = count(withoutRain, '"kind":"polygon"');
asserts.isequal(withRainPolygons, withoutRainPolygons);
r.Visible = 'off';
drawnow();
asserts.isequal(withoutRainPolygons - count(__web_display_list__(f), '"kind":"polygon"'), 2);
close(f);
%=============================================================================
% Cloud density support: [min - 3h, max + 3h], h = MAD / 0.6745 * (4 / (3 n))^(1/5).
f = figure('Visible', 'off');
r = raincloudplot([1 2 2 3 4]);
axis tight;
drawnow();
asserts.isapprox(gca().XLim, [-2.414546624 7.414546624], 1e-9);
close(f);
%=============================================================================
% Large data stays usable.
f = figure('Visible', 'off');
r = raincloudplot(randn(1e5, 1));
drawnow();
asserts.isequal(numel(r.YData), 1e5);
close(f);
%=============================================================================
asserts.checkerror('raincloudplot();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('raincloudplot({1});', getString(message('Nelson:validators:mustBeNumeric', 'ydata')));
asserts.checkerror('raincloudplot({1});', getString(message('Nelson:validators:mustBeNumeric', 'ydata')), 'Nelson:validators:mustBeNumeric');
asserts.checkerror('raincloudplot([1 2], [1 2 3]);', getString(message('Nelson:graphics:invalidGroupData')));
asserts.checkerror('raincloudplot([1 2], [1 2 3]);', getString(message('Nelson:graphics:invalidGroupData')), 'Nelson:graphics:invalidGroupData');
asserts.checkerror('raincloudplot({''a'', ''b''}, [1 2]);', getString(message('Nelson:raincloudplot:invalidGroupData')), 'Nelson:raincloudplot:invalidGroupData');
asserts.checkerror('raincloudplot([1 2], ''BadName'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'raincloudplot')));
asserts.checkerror('raincloudplot([1 2], ''BadName'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'raincloudplot')), 'Nelson:graphics:unsupportedProperty');
asserts.checkerror('raincloudplot([1 2], ''FaceAlpha'');', getString(message('Nelson:graphics:nameValueArgumentsExpected')), 'Nelson:graphics:nameValueArgumentsExpected');
asserts.checkerror('raincloudplot([1 2], ''DensityWidth'', -1);', getString(message('Nelson:graphics:mustBePositiveFinite', 'DensityWidth')));
close all
%=============================================================================
