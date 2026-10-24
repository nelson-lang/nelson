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
[T] = graphics_stackedplot_fixture();
%=============================================================================
s = stackedplot(T);
asserts.istrue(isgraphics(s, 'StackedLineChart'));
asserts.isequal(s.DisplayVariables, {'Time', 'Temperature', 'Pressure', 'Rain'});
asserts.isequal(s.DisplayLabels, {'Time'; 'Temperature'; 'Pressure'; 'Rain'});
asserts.isequal(char(s.GridVisible), 'off');
asserts.isequal(s.XLabel, 'Row');
asserts.isequal(s.XVariable, '');
asserts.isequal(length(s.Children), 4);
asserts.isequal(char(s.Children(1).Box), 'off');
% Upper panels keep the shared x-ticks so the vertical grid spans every
% panel, but hide the x-axis so only the bottom panel draws the x ruler.
asserts.istrue(~isempty(s.Children(1).XTick));
asserts.isequal(char(s.Children(1).XAxis.Visible), 'off');
asserts.isequal(char(s.Children(end).XAxis.Visible), 'on');
asserts.isequal(s.Children(1).Children(end - 1).Type, 'text');
asserts.isequal(s.Children(1).Children(end - 1).String, 'Time');
asserts.isequal(s.Children(1).Children(end - 1).Rotation, 0);
asserts.isequal(s.Children(1).Children(end - 1).Units, 'normalized');
asserts.istrue(isobject(s.AxesProperties));
asserts.isequal(fieldnames(s.AxesProperties), {'YLimits'; 'YScale'; 'LegendLabels'; ...
  'LegendLocation'; 'LegendVisible'; 'CollapseLegend'});
asserts.isequal(s.AxesProperties(1).YScale, 'linear');
asserts.isequal(s.AxesProperties(1).LegendLabels, {'Time'});
close all;
%=============================================================================
s = stackedplot(T, {'Temperature', 'Rain'}, ...
  'DisplayLabels', {'Temp', 'Rainfall'}, ...
  'GridVisible', 'on', ...
  'Title', 'Weather sample', ...
  'XLabel', 'Sample', ...
  'Color', [1 0 0], ...
  'LineWidth', 2);
asserts.isequal(s.DisplayVariables, {'Temperature', 'Rain'});
asserts.isequal(s.DisplayLabels, {'Temp'; 'Rainfall'});
asserts.isequal(char(s.GridVisible), 'on');
asserts.isequal(s.Title, 'Weather sample');
asserts.isequal(s.XLabel, 'Sample');
asserts.isequal(char(s.Children(1).XGrid), 'on');
asserts.isequal(s.Children(1).Children(end).Color, [1 0 0]);
asserts.isequal(s.Children(1).Children(end).LineWidth, 2);
asserts.isequal(fieldnames(s.LineProperties), {'Color'; 'MarkerFaceColor'; 'MarkerEdgeColor'; ...
  'LineStyle'; 'LineWidth'; 'Marker'; 'MarkerSize'; 'PlotType'});
asserts.istrue(isobject(s.LineProperties));
asserts.isequal(s.LineProperties(1).Color, [1 0 0]);
asserts.isequal(s.LineProperties(1).LineWidth, 2);
asserts.isequal(s.LineProperties(1).PlotType, 'plot');
close all;
%=============================================================================
s = stackedplot(T, {'Temperature', 'Rain'});
s.GridVisible = 'on';
asserts.isequal(char(s.Children(1).XGrid), 'on');
asserts.isequal(char(s.Children(2).YGrid), 'on');

axesProperties = s.AxesProperties;
axesProperties(1).YScale = 'log';
axesProperties(1).YLimits = [1 30];
s.AxesProperties = axesProperties;
asserts.isequal(s.Children(1).YScale, 'log');
asserts.isequal(s.Children(1).YLim, [1 30]);

s.AxesProperties(1).YScale = 'linear';
asserts.isequal(s.Children(1).YScale, 'linear');

lineProperties = s.LineProperties;
lineProperties(1).LineWidth = 3;
lineProperties(1).Marker = 'o';
s.LineProperties = lineProperties;
asserts.isequal(s.Children(1).Children(end).LineWidth, 3);
asserts.isequal(s.Children(1).Children(end).Marker, 'o');

s.LineProperties(1).LineWidth = 4;
asserts.isequal(s.Children(1).Children(end).LineWidth, 4);

s.DisplayLabels = {'Temp'; 'Rainfall'};
asserts.isequal(s.Children(1).Children(end - 1).String, 'Temp');
asserts.isequal(s.Children(2).Children(end - 1).String, 'Rainfall');

s.Title = 'Updated title';
asserts.isequal(s.Children(1).Title.String, 'Updated title');
s.XLabel = 'Updated x';
asserts.isequal(s.Children(2).XLabel.String, 'Updated x');

s.Color = [0 0 1];
asserts.isequal(s.Children(1).Children(end).Color, [0 0 1]);
s.XLimits = [1 5];
asserts.isequal(s.Children(1).XLim, [1 5]);
s.FontSize = 13;
asserts.isequal(s.Children(1).FontSize, 13);
close all;
%=============================================================================
