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
h = boxchart([1 2 3 4 100]);
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.Type, 'boxchart');
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(h.Orientation, 'vertical');
asserts.isequal(h.ColorGroupLayout, 'grouped');
asserts.isequal(char(h.Notch), 'off');
asserts.isequal(char(h.JitterOutliers), 'off');
asserts.isequal(h.XData, [1 1 1 1 1]);
asserts.isapprox(h.WhiskerLineColor, [33 33 33] ./ 255, 1e-12);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0.5 1.5]);
asserts.isequal(ax.YLim, [0 100]);
asserts.isapprox(h.BoxWidth, 0.5, 1e-12);
asserts.isapprox(h.BoxFaceAlpha, 0.2, 1e-12);
asserts.isapprox(h.CapWidth, 0.25, 1e-12);
asserts.isapprox(h.LineWidth, 1, 1e-12);
asserts.isapprox(h.MarkerSize, 6, 1e-12);
asserts.isequal(h.MarkerStyle, 'o');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxchart([1 2 3 4 12], 'BoxFaceColor', [0.2 0.5 0.8]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0.5 1.5]);
asserts.isequal(ax.YLim, [0 12]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 1 2 2 3 3];
y = [1 2 3 4 5 6];
h = boxchart(x, y, 'Notch', 'on');
ax = ancestor(h, 'axes');
asserts.isequal(h.XData, x(:));
asserts.isequal(ax.XLim, [0.5 3.5]);
asserts.isequal(ax.XTick, [0.5 1 1.5 2 2.5 3 3.5]);
asserts.isequal(char(h.Notch), 'on');
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [ones(1, 8), 2 * ones(1, 8), 3 * ones(1, 8)];
y = [1 2 3 4 5 6 7 30, 4 5 6 7 8 9 10 11, 2 3 4 5 20 21 22 23];
h = boxchart(x, y, 'Notch', 'on', 'JitterOutliers', 'on');
ax = ancestor(h, 'axes');
asserts.isequal(char(h.Notch), 'on');
asserts.isequal(char(h.JitterOutliers), 'on');
asserts.isequal(ax.XLim, [0.5 3.5]);
asserts.isequal(ax.YLim, [0 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxchart([1 2 3 4 100], 'BoxFaceColor', [0.2 0.5 0.8], ...
  'BoxFaceAlpha', 0.4, 'BoxEdgeColor', [0.1 0.1 0.1], ...
  'BoxMedianLineColor', 'r', 'MarkerColor', 'k');
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isapprox(h.BoxFaceColor, [0.2 0.5 0.8], 1e-12);
asserts.isapprox(h.BoxFaceAlpha, 0.4, 1e-12);
asserts.isequal(h.BoxFaceColorMode, 'manual');
asserts.isequal(h.BoxEdgeColorMode, 'manual');
asserts.isequal(h.BoxMedianLineColorMode, 'manual');
asserts.isequal(h.MarkerColorMode, 'manual');
ax = ancestor(h, 'axes');
asserts.isequal(ax.XLim, [0.5 1.5]);
h.BoxWidth = 0.7;
h.CapWidth = 0.3;
h.WhiskerLineStyle = '--';
h.Notch = 'on';
h.JitterOutliers = 'on';
h.MarkerStyle = 'x';
asserts.isapprox(h.BoxWidth, 0.7, 1e-12);
asserts.isapprox(h.CapWidth, 0.3, 1e-12);
asserts.isequal(h.CapWidthMode, 'manual');
asserts.isequal(h.WhiskerLineStyle, '--');
asserts.isequal(char(h.Notch), 'on');
asserts.isequal(char(h.JitterOutliers), 'on');
asserts.isequal(h.MarkerStyle, 'x');
close(f);
%=============================================================================
f = figure('Visible', 'off');
g = {'A', 'A', 'A', 'B', 'B', 'B'};
y = [1 2 3 4 5 30];
h = boxchart(g, y, 'Orientation', 'horizontal', 'BoxWidth', 0.6, ...
  'WhiskerLineColor', [0.2 0.2 0.2]);
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.Orientation, 'horizontal');
asserts.isapprox(h.BoxWidth, 0.6, 1e-12);
ax = ancestor(h, 'axes');
asserts.isequal(ax.YTickLabel, {'A'; 'B'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = boxchart(ax, magic(4), 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.Parent, ax);
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(ax.XLim, [0.5 4.5]);
asserts.isequal(size(h.YData), [4 4]);
asserts.isapprox(h.LineWidth, 2, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxchart(magic(10));
ax = ancestor(h, 'axes');
asserts.isequal(h.XData, 1:10);
asserts.isequal(ax.XLim, [0.5 10.5]);
asserts.isequal(ax.YLim, [0 100]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 1 2 2 2 1];
y = [10 20 3 4 100 15];
c = categorical({'red', 'blue', 'red', 'blue', 'red', 'blue'});
b = boxchart(x, y, 'GroupByColor', c);
asserts.isequal(numel(b), 2);
asserts.isequal(b(1).DisplayName, 'blue');
asserts.isequal(b(2).DisplayName, 'red');
asserts.isapprox(b(1).BoxWidth, 0.5, 1e-12);
asserts.isapprox(b(1).ColorGroupWidth, 4 / 7, 1e-12);
ax = ancestor(b(1), 'axes');
asserts.isequal(ax.XLim, [0.5 2.5]);
asserts.isequal(ax.YLim, [0 100]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 1 2 2 2 1];
y = [10 20 3 4 100 15];
c = {'red', 'blue', 'red', 'blue', 'red', 'blue'};
b = boxchart(x, y, 'GroupByColor', c, 'Notch', 'on', 'JitterOutliers', 'on');
asserts.isequal(numel(b), 2);
asserts.istrue(isgraphics(b(1), 'boxchart'));
asserts.istrue(isgraphics(b(2), 'boxchart'));
asserts.isequal(b(1).DisplayName, 'blue');
asserts.isequal(b(2).DisplayName, 'red');
asserts.isequal(b(1).SeriesIndex, 1);
asserts.isequal(b(2).SeriesIndex, 2);
asserts.isequal(b(1).XData(:).', [1 2 1]);
asserts.isequal(b(2).XData(:).', [1 2 2]);
ax = ancestor(b(1), 'axes');
asserts.isequal(ax.XLim, [0.5 2.5]);
asserts.isequal(ax.XTick, [0.5 1 1.5 2 2.5]);
asserts.isequal(char(b(1).Notch), 'on');
asserts.isequal(char(b(2).JitterOutliers), 'on');
close(f);
%=============================================================================
f = figure('Visible', 'off');
G = {'A'; 'A'; 'B'; 'B'};
Y = [1; 3; 2; 10];
C = {'low'; 'high'; 'low'; 'high'};
T = table(G, Y, C, 'VariableNames', {'G', 'Y', 'C'});
h = boxchart(T, 'G', 'Y');
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.XVariable, 'G');
asserts.isequal(h.YVariable, 'Y');
asserts.istrue(istable(h.SourceTable));
ax = ancestor(h, 'axes');
asserts.isequal(ax.XTickLabel, {'A'; 'B'});
b = boxchart(T, 'G', 'Y', 'GroupByColor', 'C');
asserts.isequal(numel(b), 2);
asserts.isequal(b(1).DisplayName, 'high');
asserts.isequal(b(2).DisplayName, 'low');
asserts.isequal(b(1).XData(:).', [1 2]);
asserts.isequal(b(2).XData(:).', [1 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
T = table([1; 2; 3], [4; 5; 6], 'VariableNames', {'A', 'B'});
h = boxchart(T, {'A', 'B'});
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.YVariable, 'A,B');
asserts.isequal(size(h.YData), [3 2]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XTickLabel, {'A'; 'B'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxchart(T, [true true]);
asserts.istrue(isgraphics(h, 'boxchart'));
asserts.isequal(h.YVariable, 'A,B');
asserts.isequal(size(h.YData), [3 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = boxchart([1 2 3]);
p = properties(h);
asserts.istrue(any(strcmp(p, 'BoxMedianLineColor')));
asserts.istrue(any(strcmp(p, 'WhiskerLineStyle')));
asserts.istrue(any(strcmp(p, 'ColorGroupWidthMode')));
asserts.istrue(isprop(h, 'Children'));
asserts.istrue(isValidGraphicsProperty('boxchart', 'BoxWidth'));
asserts.istrue(isValidGraphicsProperty('boxchart', 'MarkerStyle'));
asserts.isfalse(isValidGraphicsProperty('boxchart', 'NoSuchProperty'));
h.SeriesIndex = 'none';
asserts.isequal(h.SeriesIndex, 'none');
close(f);
%=============================================================================
asserts.checkerror('boxchart();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('boxchart({1, 2});', getString(message('Nelson:validators:mustBeNumeric', 'Value')));
asserts.checkerror('boxchart({''A''}, [1 2]);', getString(message('Nelson:graphics:invalidGroupData')));
asserts.checkerror('boxchart([1 2], ''BoxFaceAlpha'', 2);', getString(message('Nelson:boxchart:mustBeInRange', 'BoxFaceAlpha')));
asserts.checkerror('boxchart([1 2], ''BoxWidth'', 0);', getString(message('Nelson:graphics:mustBePositiveFinite', 'BoxWidth')));
asserts.checkerror('boxchart([1 2], ''NotAProperty'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'boxchart')));
%=============================================================================
