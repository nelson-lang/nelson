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
f = figure('Visible', 'off');
h = fimplicit(@(x, y) x .^ 2 + y .^ 2 - 1, [-2 2 -2 2], '--r', 'MeshDensity', 51);
ax = ancestor(h, 'axes');
asserts.istrue(isgraphics(h, 'implicitfunctionline'));
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.Type, 'implicitfunctionline');
asserts.isequal(h.XRange, [-2 2]);
asserts.isequal(h.YRange, [-2 2]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.YRangeMode, 'manual');
asserts.istrue(isnumeric(h.XData));
asserts.istrue(isnumeric(h.YData));
asserts.istrue(isnumeric(h.ZData));
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Marker, 'none');
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [-2 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fimplicit(@(x, y) x .^ 2 + y .^ 2 - 1, [-2 2 -2 2]);
asserts.isequal(h.MeshDensity, 151);
asserts.isequal(h.Color, [0.066 0.443 0.745]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fimplicit(ax, @(x, y) scalar_circle(x, y), 'ob', [-2 2], 'LineWidth', 2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Type, 'implicitfunctionline');
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.LineStyle, 'none');
close(f);
%=============================================================================
f = figure('Visible', 'off');
f1 = @(x, y) x .^ 2 + y .^ 2 - 1;
f2 = @(x, y) x - y;
h = fimplicit({f1, f2});
asserts.isequal(size(h), [2 1]);
asserts.isequal(h(1).Type, 'implicitfunctionline');
asserts.isequal(h(2).Type, 'implicitfunctionline');
asserts.isequal(h(1).Parent, h(2).Parent);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fimplicit(@(x, y) x, [1 1]);
asserts.isequal(h.XRange, [1 1]);
asserts.isequal(h.YRange, [1 1]);
close(f);
%=============================================================================
asserts.checkerror('fimplicit(1);', getString(message('Nelson:graphics:functionHandleExpected', 'Value')));
asserts.checkerror('fimplicit(@(x, y) x, @(x, y) y);', getString(message('Nelson:graphics:tooManyFunctions')));
asserts.checkerror('fimplicit(@(x, y) x, ''BadName'', 1);', getString(message('Nelson:graphics:invalidParameter', 'BadName')));
asserts.checkerror('fimplicit(@(x, y) x, ''badstyle'');', getString(message('Nelson:graphics:invalidParameter', 'badstyle')));
%=============================================================================
function z = scalar_circle(x, y)
  if numel(x) ~= 1 || numel(y) ~= 1
    error('scalar only');
  end
  z = x .^ 2 + y .^ 2 - 1;
end
%=============================================================================
