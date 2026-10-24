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
h = fcontour(@(x, y) x .^ 2 - y .^ 2, [-2 2 -1 1]);
asserts.istrue(isgraphics(h, 'functioncontour'));
asserts.isequal(h.Type, 'functioncontour');
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.XRange, [-2 2]);
asserts.isequal(h.YRange, [-1 1]);
asserts.isequal(h.MeshDensity, 71);
asserts.isequal(h.LineColor, 'flat');
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(size(h.XData), [71 71]);
asserts.isequal(size(h.YData), [71 71]);
asserts.isequal(size(h.ZData), [71 71]);
asserts.isapprox(h.XData(1, 1), -2, 1e-12);
asserts.isapprox(h.YData(1, 1), -1, 1e-12);
asserts.isequal(h.LevelList, [-1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4]);
asserts.isapprox(gca().XLim, [-2 2], 1e-12);
asserts.isapprox(gca().YLim, [-1 1], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fcontour(ax, @(x, y) scalar_surface(x, y), [-1 1], 'LineColor', 'red');
asserts.isequal(h.Parent, ax);
asserts.isequal(size(h.ZData), [71 71]);
asserts.isequal(h.LineColor, [1 0 0]);
h.MeshDensity = 9;
asserts.isequal(size(h.XData), [9 9]);
asserts.isequal(size(h.ZData), [9 9]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fcontour(@(x, y) x + y, '-r', 'MeshDensity', 21, 'LevelList', [-1 0 1]);
asserts.isequal(h.Type, 'functioncontour');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.MeshDensity, 21);
asserts.isequal(h.LevelListMode, 'manual');
asserts.isequal(h.LevelList, [-1 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
f1 = @(x, y) x + y;
f2 = @(x, y) x - y;
h = fcontour({f1, f2});
asserts.isequal(size(h), [2 1]);
asserts.istrue(all(isgraphics(h, 'functioncontour')));
asserts.isequal(h(1).LineStyle, '-');
asserts.isequal(h(2).LineStyle, '-');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fcontour(@(x, y) x, [1 1]);
asserts.isequal(h.XRange, [1 1]);
asserts.isequal(h.YRange, [1 1]);
asserts.isequal(size(h.XData), [71 71]);
close(f);
%=============================================================================
asserts.checkerror('fcontour(1);', getString(message('Nelson:graphics:functionHandleExpected', 'Value')));
asserts.checkerror('fcontour(@(x, y) x + y, ''-or'');', getString(message('Nelson:fcontour:markersNotSupported')));
asserts.checkerror('fcontour(@(x, y) x, @(x, y) y);', getString(message('Nelson:graphics:tooManyFunctions')));
asserts.checkerror('fcontour(@(x, y) x + y, ''BadName'', 1);', getString(message('Nelson:graphics:invalidParameter', 'BadName')));
%=============================================================================
function z = scalar_surface(x, y)
  if numel(x) ~= 1 || numel(y) ~= 1
    error('scalar only');
  end
  z = x + y;
end
%=============================================================================
