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
h = area([1 3 2]);
asserts.istrue(isgraphics(h, 'area'));
asserts.isequal(h.Type, 'area');
asserts.isequal(h.BaseValue, 0);
asserts.isequal(char(h.ShowBaseLine), 'on');
asserts.isequal(size(h.Vertices), [6 3]);
%=============================================================================
f = figure('Visible', 'off');
y = [1 2; 3 1; 2 4];
h = area(y);
drawnow();
ax = gca();
asserts.isequal(size(h), [1 2]);
asserts.istrue(isgraphics(h(1), 'area'));
asserts.istrue(isgraphics(h(2), 'area'));
asserts.isequal(h(1).YData, y(:, 1));
asserts.isequal(h(2).YData, y(:, 2));
asserts.isequal(h(2).BaseValue, 0);
asserts.isequal(ax.XLim, [1 3]);
asserts.isequal(ax.YLim, [0 6]);
asserts.isapprox(ax.XTick, 1:0.2:3, 1e-12);
%=============================================================================
f = figure('Visible', 'off');
x = [10; 20; 30];
y = [1; 2; 3];
h = area(x, y, -1, 'FaceAlpha', 0.25, 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'area'));
asserts.isequal(h.BaseValue, -1);
asserts.isapprox(h.FaceAlpha, 0.25, 1e-12);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.XData, x);
asserts.isequal(h.YData, y);
%=============================================================================
asserts.istrue(isValidGraphicsProperty('area', 'BaseValue'));
asserts.istrue(isValidGraphicsProperty('area', 'ShowBaseLine'));
asserts.istrue(isValidGraphicsProperty('area', 'FaceColorMode'));
%=============================================================================
