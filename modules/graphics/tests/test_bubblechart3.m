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
h = bubblechart3(1:3, [3 2 1], [0 1 2], [20 40 60], 'b');
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(h.Type, 'bubblechart');
asserts.isequal(h.XData, [1 2 3]);
asserts.isequal(h.YData, [3 2 1]);
asserts.isequal(h.ZData, [0 1 2]);
asserts.isequal(h.SizeData, [20 40 60]);
asserts.isequal(h.CData, [0 0 1]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.BoxStyle, 'back');
asserts.isapprox(ax.XLim, [0.698736859666861 3.30126314033314], 1e-12);
asserts.isapprox(ax.YLim, [0.604557729984015 3.39544227001599], 1e-12);
asserts.isapprox(ax.ZLim, [-0.452914798206278 2.45291479820628], 1e-12);
asserts.isapprox(ax.XTick, 1:0.5:3, 1e-12);
asserts.isapprox(ax.YTick, 1:0.5:3, 1e-12);
asserts.isapprox(ax.ZTick, 0:0.5:2, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = bubblechart3(ax, 1:2, 3:4, 5:6, [10 20], 'MarkerEdgeColor', 'k', ...
  'MarkerFaceAlpha', 0.25, 'MarkerEdgeAlpha', 0.75, 'LineWidth', 2, ...
  'DisplayName', 'series a', 'ZDataSource', 'z', ...
  'YJitterWidth', 0.3, 'YJitterWidthMode', 'manual');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.MarkerEdgeColor, [0 0 0]);
asserts.isapprox(h.MarkerFaceAlpha, 0.25, 1e-12);
asserts.isapprox(h.MarkerEdgeAlpha, 0.75, 1e-12);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.DisplayName, 'series a');
asserts.isequal(h.ZDataSource, 'z');
asserts.isapprox(h.YJitterWidth, 0.3, 1e-12);
asserts.isequal(h.YJitterWidthMode, 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = [1 2; 3 4; 5 6];
y = x + 10;
z = x + 20;
s = x + 30;
h = bubblechart3(x, y, z, s);
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).XData, [1 3 5]);
asserts.isequal(h(2).XData, [2 4 6]);
asserts.isequal(h(1).YData, [11 13 15]);
asserts.isequal(h(2).ZData, [22 24 26]);
asserts.isequal(h(1).SizeData, [31 33 35]);
asserts.isequal(h(2).SizeData, [32 34 36]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblechart3(1:3, 2:4, 3:5, 10, '#8000FF');
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).XData, 1);
asserts.isequal(h(2).YData, 3);
asserts.isequal(h(3).ZData, 5);
asserts.isequal(h(1).SizeData, 10);
asserts.isapprox(h(1).CData, [0.5020 0 1.0000], 1e-4);
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table((1:4)', [4; 2; 6; 3], [7; 8; 9; 10], [20; 60; 30; 80], [1; 2; 3; 4], ...
  'VariableNames', {'x', 'y', 'z', 's', 'c'});
h = bubblechart3(t, 'x', 'y', 'z', 's', 'c', 'DisplayName', 'from table');
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XData, [1 2 3 4]);
asserts.isequal(h.YData, [4 2 6 3]);
asserts.isequal(h.ZData, [7 8 9 10]);
asserts.isequal(h.SizeData, [20 60 30 80]);
asserts.isequal(h.CData(:).', [1 2 3 4]);
asserts.isequal(h.XVariable, 'x');
asserts.isequal(h.YVariable, 'y');
asserts.isequal(h.ZVariable, 'z');
asserts.isequal(h.SizeVariable, 's');
asserts.isequal(h.ColorVariable, 'c');
asserts.isequal(h.DisplayName, 'from table');
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table((1:3)', [2; 4; 6], [3; 6; 9], [10; 11; 12], [10; 20; 30], ...
  'VariableNames', {'x', 'y1', 'y2', 'z', 's'});
h = bubblechart3(t, 'x', {'y1', 'y2'}, 'z', 's');
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).YData, [2 4 6]);
asserts.isequal(h(2).YData, [3 6 9]);
asserts.isequal(h(1).ZData, [10 11 12]);
asserts.isequal(h(2).ZData, [10 11 12]);
asserts.isequal(h(1).YVariable, 'y1');
asserts.isequal(h(2).YVariable, 'y2');
asserts.isequal(h(1).ZVariable, 'z');
asserts.isequal(h(2).ZVariable, 'z');
close(f);
%=============================================================================
asserts.checkerror('bubblechart3(1, 2, 3);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('bubblechart3({1}, 2, 3, 4);', getString(message('Nelson:bubblechart3:numericXYExpected')));
asserts.checkerror('bubblechart3(1:3, [3 2 1], [0 1 2], [20 40 60], ''BubbleColor'', ''b'');', _('Unrecognized property BubbleColor for class BubbleChart.'));
asserts.checkerror('bubblechart3(1:2, 1:3, 1:2, [10 20]);', getString(message('Nelson:dimagree:xYZSize')));
asserts.checkerror('bubblechart3(1:3, 1:3, 1:3, [10 20]);', getString(message('Nelson:dimagree:sizeSMatchXYZ')));
asserts.checkerror('bubblechart3([1 2; 3 4], [1 2; 3 4], [1 2; 3 4], 10);', getString(message('Nelson:dimagree:sizeSMatchXYZ')));
%=============================================================================
