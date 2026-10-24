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
h = fplot3(@(t) cos(t), @(t) sin(t), @(t) t, [0 2 * pi]);
asserts.istrue(isgraphics(h, 'parameterizedfunctionline'));
asserts.isequal(h.Type, 'parameterizedfunctionline');
asserts.isequal(h.TRange, [0 2 * pi]);
asserts.isequal(h.TRangeMode, 'manual');
asserts.isequal(h.MeshDensity, 15);
asserts.isequal(size(h.XData), [1 217]);
asserts.isequal(size(h.YData), [1 217]);
asserts.isequal(size(h.ZData), [1 217]);
asserts.isapprox(h.XData(1), 1, 1e-12);
asserts.isapprox(h.YData(1), 0, 1e-12);
asserts.isapprox(h.ZData(1), 0, 1e-12);
asserts.isapprox(h.ZData(end), 2 * pi, 1e-12);
asserts.isequal(h.Color, [0.066 0.443 0.745]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [-37.5 30]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fplot3(@(t) t, @(t) t .^ 2, @(t) t .^ 3, [-2 2], 'r--', 'LineWidth', 2);
ax = ancestor(h, 'axes');
asserts.isequal(ax.XTick, -2:1:2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = fplot3(ax, @(t) t, @(t) t .^ 2, @(t) t .^ 3, [-1 1], 'r--', 'LineWidth', 2, 'MeshDensity', 19);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.TRange, [-1 1]);
asserts.isequal(h.MeshDensity, 19);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fplot3(@(t) scalar_square(t), @(t) scalar_square(t) + 1, @(t) 2, [0 1]);
asserts.isequal(numel(h.XData), 29);
asserts.isequal(h.ZData, 2 * ones(1, 29));
close(f);
%=============================================================================
asserts.throws('fplot3(@sin, @cos);', 'argument', 'Nelson:narginchk:notEnoughInputs');
asserts.throws('fplot3(@sin, @cos, 1);', 'Value', 'Nelson:graphics:functionHandleExpected');
h = fplot3(@sin, @cos, @tan, [1 1]);
asserts.isequal(h.TRange, [1 1]);
close(gcf());
asserts.throws('fplot3(@sin, @cos, @tan, @exp);', 'on', 'Nelson:graphics:tooManyFunctions');
asserts.throws('fplot3(@sin, @cos, @tan, ''BadName'', 1);', 'BadName', 'Nelson:graphics:invalidParameter');
%=============================================================================
function y = scalar_square(x)
  if numel(x) ~= 1
    error('scalar only');
  end
  y = x .^ 2;
end
%=============================================================================
