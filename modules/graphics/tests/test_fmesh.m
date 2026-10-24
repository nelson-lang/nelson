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
h = fmesh(@(x, y) x .^ 2 + y .^ 2, [-1 1 -2 2], 'MeshDensity', 17);
ax = ancestor(h, 'axes');
asserts.istrue(isgraphics(h, 'functionsurface'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.Type, 'functionsurface');
asserts.isequal(h.XRange, [-1 1]);
asserts.isequal(h.YRange, [-2 2]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.YRangeMode, 'manual');
asserts.isequal(h.MeshDensity, 17);
asserts.istrue(isnumeric(h.XData));
asserts.istrue(isnumeric(h.YData));
asserts.istrue(isnumeric(h.ZData));
asserts.isapprox(h.XData(1, 1), -1, 1e-12);
asserts.isapprox(h.YData(1, 1), -2, 1e-12);
asserts.isapprox(h.ZData(1, 1), 5, 1e-12);
asserts.isequal(h.FaceColor, [1 1 1]);
asserts.isequal(h.EdgeColor, 'interp');
asserts.isequal(h.LineStyle, '-');
asserts.isequal(ax.XLim, [-1 1]);
asserts.isequal(ax.YLim, [-2 2]);
asserts.isequal(ax.View, [-37.5 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fmesh(ax, @(x, y) scalar_surface(x, y), [-1 1], 'LineWidth', 2, 'EdgeColor', 'b');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.EdgeColor, [0 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fmesh(@(x, y) x + y, [-2 2 -3 3], 'Parent', ax, 'MeshDensity', 11);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.XRange, [-2 2]);
asserts.isequal(h.YRange, [-3 3]);
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-3 3]);
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(h.MeshDensity, 11);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fmesh(@(x, y) x, @(x, y) y, @(x, y) x + y, [-1 1 -2 2], 'MeshDensity', 9);
asserts.isequal(h.Type, 'functionsurface');
asserts.isequal(h.MeshDensity, 9);
asserts.isapprox(h.XData(1, 1), -1, 1e-12);
asserts.isapprox(h.YData(1, 1), -2, 1e-12);
asserts.isapprox(h.ZData(1, 1), -3, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fmesh(1);
asserts.isequal(h.Type, 'functionsurface');
asserts.istrue(all(h.ZData(:) == 1));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fmesh(@(x, y) x, [1 1]);
asserts.isequal(h.XRange, [1 1]);
asserts.isequal(h.YRange, [1 1]);
close(f);
%=============================================================================
asserts.checkerror('fmesh(@(x, y) x, [1 2 3]);', getString(message('Nelson:graphics:invalidParameter', '[1 2 3]')));
asserts.checkerror('fmesh(@(x, y) x, @(x, y) y);', getString(message('Nelson:graphics:tooManyFunctions')));
asserts.checkerror('fmesh(@(x, y) x, ''BadName'', 1);', getString(message('Nelson:graphics:invalidParameter', 'BadName')));
%=============================================================================
function z = scalar_surface(x, y)
  if numel(x) ~= 1 || numel(y) ~= 1
    error('scalar only');
  end
  z = x + y;
end
%=============================================================================
