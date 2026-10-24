%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/975
% <-- Short Description -->
% Legend color is not matching that of curve in figure.
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
a = [0:0.5:5];
b = 2 * a .^ 2 + 3 * a - 5;
c = 1.2 * a .^ 2 + 4 * a - 3;
subplot(1, 2, 1)
plot(a, b, '-or', 'MarkerFaceColor', 'g', 'LineWidth', 2)
xlabel('X'); ylabel('Y'); legend('Curve ', 'Location', 'NorthWest')
subplot(1, 2, 2)
plot(a, c, '--ok', 'MarkerFaceColor', 'c', 'LineWidth', 2)
xlabel('X'); ylabel('Y'); legend('Curve 2', 'Location', 'NorthWest')
%=============================================================================
f = figure('Visible', 'off');
a = [0:0.5:5];
b = 2 * a .^ 2 + 3 * a - 5;
h = plot(a, b, '-or', 'MarkerFaceColor', 'g', 'LineWidth', 3);
[lgd, ~, plotHandles] = legend('Curve ', 'Location', 'NorthWest');
asserts.istrue(isgraphics(lgd, 'legend'));
asserts.isequal(lgd.String, {'Curve '});
asserts.isequal(lgd.Location, 'northwest');
asserts.isequal(plotHandles, h);
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.MarkerFaceColor, [0 1 0]);
asserts.isequal(h.LineWidth, 3);
%=============================================================================
