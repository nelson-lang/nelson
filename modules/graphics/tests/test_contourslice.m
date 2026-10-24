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
[x, y, z] = meshgrid(-2:2, -2:2, -2:2);
v = x .^ 2 + y .^ 2 + z .^ 2;
f = figure('Visible', 'off');
h = contourslice(x, y, z, v, 0, [], 0);
asserts.istrue(all(isgraphics(h, 'patch')));
asserts.isequal(size(h), [32 1]);
asserts.isequal(h(1).Parent.Type, 'axes');
asserts.isequal(h(1).FaceColor, 'none');
asserts.isequal(h(1).EdgeColor, 'flat');
asserts.istrue(isnumeric(h(1).UserData));
xdata = h(1).XData;
ydata = h(1).YData;
zdata = h(1).ZData;
asserts.istrue(isnan(xdata(end)));
asserts.istrue(isnan(ydata(end)));
asserts.istrue(isnan(zdata(end)));
asserts.isequal(size(h(1).CData), [6 1]);
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.View, [0 90]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = contourslice(ax, v, 2, [], 2, [2 4 6]);
asserts.isequal(h(1).Parent, ax);
asserts.istrue(numel(h) > 0);
asserts.isequal(h(1).Type, 'patch');
asserts.istrue(ismember(h(1).UserData, [2 4 6]));
asserts.isequal(size(h(1).CData, 1), length(h(1).XData));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = contourslice(v, NaN, [], [], 4, 'nearest');
asserts.istrue(all(isgraphics(h, 'patch')));
asserts.isequal(size(h), [50 1]);
asserts.isequal(h(1).UserData, 5.6);
asserts.isequal(size(h(1).CData), [14 1]);
close(f);
%=============================================================================
[x, y, z] = meshgrid(-2:2, -2:2, -2:2);
v = x .^ 2 + 2 .* y + z;
[xi, yi] = meshgrid(-2:1:2, -2:1:2);
zi = 0.5 .* xi;
f = figure('Visible', 'off');
h = contourslice(x, y, z, v, xi, yi, zi, [-2 0 2]);
asserts.isequal(size(h), [3 1]);
asserts.isequal(h(1).Type, 'patch');
asserts.isequal(h(1).UserData, -2);
asserts.isequal(size(h(1).CData), [6 1]);
close(f);
%=============================================================================
asserts.checkerror('contourslice(ones(2), 1, 1, 1);', getString(message('Nelson:graphics:invalidVolumeData')));
asserts.checkerror('contourslice(ones(2,2,2), {1}, [], []);', getString(message('Nelson:validators:mustBeNumeric', 'Slice locations')));
asserts.checkerror('contourslice(ones(2,2,2), 1, 1, 1, ''bad'');', getString(message('Nelson:contourslice:invalidInterpolationMethod')));
%=============================================================================
f = figure('Visible', 'off', 'Position', [100, 100, 560, 420]);
[X, Y, Z] = meshgrid(-2:.2:2);
V = X .* exp(-X .^ 2 - Y .^ 2 - Z .^ 2);
xslice = [-1.2, 0.8, 2];
levels = -0.2:0.01:0.4;
h = contourslice(X, Y, Z, V, xslice, [], [], levels);
cb = colorbar;
view(3);
grid on;
ax = gca();
asserts.istrue(all(strcmp({h.Tag}, 'contourslice')));
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-2 2]);
asserts.isequal(ax.ZLim, [-2 2]);
asserts.isequal(ax.XTick, -2:1:2);
asserts.isequal(ax.YTick, -2:1:2);
asserts.isequal(ax.ZTick, -2:1:2);
asserts.isapprox(ax.Position, [0.114642857142857 0.11 0.683428571428571 0.815], 1e-12);
asserts.isapprox(cb.Position, [0.830303571428571 0.11 0.0380952380952381 0.815], 1e-12);
asserts.isapprox(cb.Ticks, -0.2:0.1:0.4, 1e-12);
close(f);
%=============================================================================
