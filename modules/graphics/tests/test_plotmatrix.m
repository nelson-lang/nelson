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
X = [1 2 3; 2 3 5; 3 5 8; 4 7 13];
[h, ax, bigax, p] = plotmatrix(X);
asserts.isequal(size(h), [3 3]);
asserts.isequal(size(ax), [3 3]);
asserts.istrue(isgraphics(h(1, 1), 'line'));
asserts.istrue(isgraphics(h(1, 2), 'line'));
asserts.istrue(isgraphics(bigax, 'axes'));
asserts.isequal(char(bigax.Visible), 'off');
asserts.isequal(bigax.Tag, 'PlotMatrixBigAx');
asserts.isequal(ax(1, 1).Tag, 'PlotMatrixScatterAx');
asserts.isapprox(ax(1, 1).Position, ...
  [0.135166666666667 0.658766666666667 0.253166666666667 0.266233333333333], 1e-12);
asserts.isapprox(ax(1, 1).XLim, [0.4 4.6], 1e-12);
asserts.isapprox(ax(1, 2).XLim, [1.25 7.75], 1e-12);
asserts.istrue(isempty(ax(3, 1).XLabel.String));
asserts.istrue(isempty(ax(3, 1).YLabel.String));
asserts.isequal(bigax.UserData.ChartType, 'plotmatrix');
asserts.isequal(bigax.UserData.SameData, true);
asserts.isequal(numel(p), 3);
asserts.istrue(isgraphics(p(1), 'histogram'));
[~, ~, ~, ~, pax] = plotmatrix(X);
asserts.isequal(size(pax), [1 3]);
asserts.istrue(isgraphics(pax(1), 'axes'));
close(f);
%=============================================================================
f = figure('Visible', 'off');
X = [1 2; 3 4; 5 6];
Y = [6 5 4; 3 2 1; 1 3 5];
[h, ax, bigax] = plotmatrix(X, Y, 'o');
asserts.isequal(size(h), [3 2]);
asserts.isequal(size(ax), [3 2]);
asserts.istrue(isgraphics(h(1, 1), 'line'));
asserts.isequal(bigax.UserData.SameData, false);
asserts.isequal(bigax.UserData.XData, double(X));
asserts.isequal(bigax.UserData.YData, double(Y));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax0 = axes();
[h, ax, bigax] = plotmatrix(ax0, X);
asserts.isequal(bigax, ax0);
asserts.isequal(char(bigax.Visible), 'off');
asserts.istrue(isgraphics(h(1, 1), 'line'));
close(f);
%=============================================================================
asserts.checkerror('plotmatrix({1, 2});', getString(message('Nelson:plotmatrix:numericMatrixExpected', 'Value')));
asserts.checkerror('plotmatrix([1 2; 3 4], [1 2 3]);', getString(message('Nelson:dimagree:xYNumberRows')));
asserts.checkerror('plotmatrix([1 2; 3 4], ''o'', ''extra'');', getString(message('Nelson:graphics:unsupportedProperty', 'plotmatrix')));
%=============================================================================
