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
C = [1 2 3; 4 5 6];
h = heatmap(C);
asserts.istrue(isgraphics(h, 'heatmap'));
asserts.isequal(h.Type, 'heatmap');
asserts.isequal(h.ColorData, C);
asserts.isequal(h.XData, {'1'; '2'; '3'});
asserts.isequal(h.YData, {'1'; '2'});
asserts.isequal(h.XDisplayLabels, {'1'; '2'; '3'});
asserts.isequal(h.YDisplayLabels, {'1'; '2'});
asserts.isequal(char(h.GridVisible), 'on');
asserts.isequal(char(h.ColorbarVisible), 'on');
asserts.isequal(h.CellLabelFormat, '%0.4g');
s = h.UserData;
asserts.isequal(s.ChartType, 'heatmap');
asserts.istrue(isgraphics(s.Image, 'image'));
asserts.isequal(s.Image.CData, C);
asserts.isequal(s.Image.CDataMapping, 'scaled');
asserts.istrue(isempty(s.MissingCells));
asserts.isequal(numel(s.CellLabels), numel(C));
asserts.isequal(numel(s.Grid), size(C, 1) + size(C, 2) + 2);
asserts.istrue(isgraphics(s.Colorbar, 'colorbar'));
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0.5 3.5]);
asserts.isequal(ax.YLim, [0.5 2.5]);
asserts.isequal(ax.XTick, 1:3);
asserts.isequal(ax.YTick, 1:2);
asserts.isequal(ax.XTickLabel, {'1'; '2'; '3'});
asserts.isequal(ax.YTickLabel, {'1'; '2'});
asserts.isequal(ax.YDir, 'reverse');
asserts.isequal(ax.TickLength, [0 0]);
asserts.istrue(isprop(h, 'Units'));
asserts.isequal(h.Units, 'normalized');
asserts.istrue(isprop(h, 'Position'));
asserts.isequal(h.Position, [0.13 0.11 0.775 0.815]);
asserts.istrue(isprop(h, 'InnerPosition'));
asserts.isequal(h.InnerPosition, [0.13 0.11 0.775 0.815]);
asserts.istrue(isprop(h, 'OuterPosition'));
asserts.isequal(h.OuterPosition, [0 0 1 1]);
asserts.istrue(isprop(h, 'FontName'));
asserts.isequal(h.FontName, 'Helvetica');
asserts.isequal(get(h, 'Units'), 'normalized');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = heatmap({'A', 'B', 'C'}, {'R1', 'R2'}, C, ...
  'Title', 'Demo', 'XLabel', 'Columns', 'YLabel', 'Rows', ...
  'ColorLimits', [0 10], 'ColorbarVisible', 'off', 'GridVisible', 'off');
s = h.UserData;
ax = ancestor(h, 'axes');
asserts.isequal(h.Title, 'Demo');
asserts.isequal(h.XLabel, 'Columns');
asserts.isequal(h.YLabel, 'Rows');
asserts.isequal(h.XData, {'A'; 'B'; 'C'});
asserts.isequal(h.YData, {'R1'; 'R2'});
asserts.isequal(h.ColorLimits, [0 10]);
asserts.isequal(char(h.ColorbarVisible), 'off');
asserts.isequal(char(h.GridVisible), 'off');
asserts.isequal(s.XData, {'A'; 'B'; 'C'});
asserts.isequal(s.YData, {'R1'; 'R2'});
asserts.istrue(isempty(s.MissingCells));
asserts.isequal(ax.XTickLabel, {'A'; 'B'; 'C'});
asserts.isequal(ax.YTickLabel, {'R1'; 'R2'});
asserts.isequal(ax.CLim, [0 10]);
asserts.istrue(isempty(s.Colorbar));
asserts.istrue(isempty(s.Grid));
asserts.isequal(ax.Title.String, 'Demo');
asserts.isequal(ax.XLabel.String, 'Columns');
asserts.isequal(ax.YLabel.String, 'Rows');
close(f);
%=============================================================================
T = table({'B'; 'A'; 'B'}, {'Y'; 'X'; 'X'}, [2; 5; 8], ...
  'VariableNames', {'x', 'y', 'v'});
f = figure('Visible', 'off');
h = heatmap(T, 'x', 'y');
asserts.istrue(isgraphics(h, 'heatmap'));
asserts.isequal(h.Type, 'heatmap');
asserts.istrue(istable(h.SourceTable));
asserts.isequal(h.XVariable, 'x');
asserts.isequal(h.YVariable, 'y');
asserts.isequal(h.ColorVariable, '');
asserts.isequal(h.ColorMethod, 'count');
asserts.isequal(h.Title, 'Count of y vs. x');
asserts.isequal(h.XLabel, 'x');
asserts.isequal(h.YLabel, 'y');
asserts.isequal(h.XData, {'A'; 'B'});
asserts.isequal(h.YData, {'X'; 'Y'});
asserts.isequal(h.ColorData, [1 1; 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = heatmap(T, 'x', 'y', 'ColorVariable', 'v');
asserts.isequal(h.Type, 'heatmap');
asserts.istrue(istable(h.SourceTable));
asserts.isequal(h.XVariable, 'x');
asserts.isequal(h.YVariable, 'y');
asserts.isequal(h.ColorVariable, 'v');
asserts.isequal(h.ColorMethod, 'mean');
asserts.isequal(h.Title, 'Mean of v');
asserts.isequal(h.XLabel, 'x');
asserts.isequal(h.YLabel, 'y');
asserts.isequal(h.XData, {'A'; 'B'});
asserts.isequal(h.YData, {'X'; 'Y'});
asserts.isequal(h.ColorData([1 3 4]), [5 8 2]);
asserts.istrue(isnan(h.ColorData(2, 1)));
s = h.UserData;
asserts.isequal(numel(s.MissingCells), 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = heatmap(ax, [7 8; 9 10], 'XData', [10 20], 'YData', {'Low', 'High'}, ...
  'CellLabelFormat', '%0.1f', 'Colormap', gray(8));
s = h.UserData;
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Type, 'heatmap');
asserts.isequal(h.XData, {'10'; '20'});
asserts.isequal(h.YData, {'Low'; 'High'});
asserts.isequal(h.CellLabelFormat, '%0.1f');
asserts.isequal(s.XData, {'10'; '20'});
asserts.isequal(s.YData, {'Low'; 'High'});
asserts.isequal(s.CellLabels(1).String, '7.0');
asserts.isequal(size(ax.Colormap), [8 3]);
close(f);
%=============================================================================
% Cell-label contrast: the label color follows the cell background luminance.
% A cell drawn on a dark background gets white text; a cell on a light
% background keeps the dark font color.
f = figure('Visible', 'off');
h = heatmap([0 1], 'Colormap', [1 1 1; 0 0 0], 'ColorLimits', [0 1]);
s = h.UserData;
% drawCellLabels walks rows then columns: label 1 -> value 0 (white cell),
% label 2 -> value 1 (black cell).
asserts.isequal(s.CellLabels(2).Color, [1 1 1]);
asserts.isfalse(isequal(s.CellLabels(1).Color, [1 1 1]));
close(f);
%=============================================================================
% Color mapping is not compressed: the data min and max map to the color-limit
% endpoints so extreme cells reach the colormap ends. The default colormap end
% is dark enough that the maximum cell takes the white contrast label.
f = figure('Visible', 'off');
C = peaks(12);
h = heatmap(C);
ax = ancestor(h, 'axes');
asserts.isequal(ax.CLim, [min(C(:)) max(C(:))]);
asserts.isequal(h.ColorLimits, [min(C(:)) max(C(:))]);
asserts.isequal(h.UserData.Image.CDataMapping, 'scaled');
[~, imx] = max(C(:));
[ri, cj] = ind2sub(size(C), imx);
labelIdx = (ri - 1) * size(C, 2) + cj;
asserts.isequal(h.UserData.CellLabels(labelIdx).Color, [1 1 1]);
close(f);
%=============================================================================
asserts.checkerror('heatmap();', getString(message('Nelson:narginchk:notEnoughInputs')), ...
  'Nelson:narginchk:notEnoughInputs');
asserts.checkerror('heatmap({''A''}, {''B''}, {1});', ...
  getString(message('Nelson:validators:mustBeNumeric', 'Color data')), 'Nelson:validators:mustBeNumeric');
asserts.checkerror('heatmap([1 2], ''ColorLimits'', [2 1]);', ...
  getString(message('Nelson:heatmap:invalidColorLimits')), 'Nelson:heatmap:invalidColorLimits');
asserts.checkerror('heatmap({''A''}, {''B''}, [1 2]);', ...
  getString(message('Nelson:heatmap:invalidNumberOfAxis')), 'Nelson:heatmap:invalidNumberOfAxis');
%=============================================================================
