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
close all
%=============================================================================
f = figure('Visible', 'off');
h = fplot(@(x) sin(x), [0 pi], '-r', 'LineWidth', 1.5);
ax = ancestor(h, 'axes');
asserts.istrue(isgraphics(h, 'functionline'));
asserts.isequal(h.Type, 'functionline');
asserts.isequal(h.XRange, [0 pi]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.MeshDensity, 23);
asserts.isequal(size(h.XData), [1 57]);
asserts.isapprox(h.XData(1), 0, 1e-12);
asserts.isapprox(h.XData(end), pi, 1e-12);
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.Marker, 'none');
asserts.isequal(h.LineWidth, 1.5);
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.XLim, [0 pi]);
asserts.isequal(ax.ZLim, [-1 1]);
asserts.isequal(ax.ZTick, [-1 0 1]);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fplot(@scalarOnly, [0 1]);
ax = ancestor(h, 'axes');
asserts.isequal(h.Type, 'functionline');
asserts.isequal(size(h.XData), [1 45]);
asserts.isapprox(h.YData(1), 1, 1e-12);
asserts.isapprox(h.YData(end), 2, 1e-12);
asserts.isequal(ax.YLim, [1 2]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fplot(@(t) cos(t), @(t) sin(t), [0 2 * pi], 'ob', 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'parameterizedfunctionline'));
asserts.isequal(h.Type, 'parameterizedfunctionline');
asserts.isequal(h.TRange, [0 2 * pi]);
asserts.isequal(h.TRangeMode, 'manual');
asserts.isequal(h.MeshDensity, 15);
% Parametric curves are sampled with adaptive refinement, so the vertex count
% is larger than the initial mesh while the endpoints are preserved.
asserts.isequal(size(h.XData), [1 241]);
asserts.isequal(size(h.YData), [1 241]);
asserts.isapprox(h.XData(1), 1, 1e-12);
asserts.isapprox(h.YData(1), 0, 1e-12);
asserts.isapprox(h.XData(end), cos(2 * pi), 1e-12);
asserts.isapprox(h.YData(end), sin(2 * pi), 1e-12);
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.LineStyle, 'none');
asserts.isequal(h.LineWidth, 2);
ax = ancestor(h, 'axes');
axis(ax, 'equal');
drawnow();
asserts.isequal(ax.ZLim, [-1 1]);
asserts.isequal(ax.ZTick, [-1 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = fplot(1);
asserts.isequal(h.Type, 'functionline');
asserts.istrue(all(h.YData == 1));
close(f);
%=============================================================================
[x, y] = fplot(@sin, [0 1]);
asserts.isequal(size(x), [1 45]);
asserts.isequal(size(y), [1 45]);
asserts.isapprox(x(1), 0, 1e-12);
asserts.isapprox(x(end), 1, 1e-12);
%=============================================================================
asserts.throws('fplot(@sin, [1 0])', 'Range', 'Nelson:fplot:invalidRangeValue');
asserts.throws('fplot(@sin, @cos, @tan)', 'on', 'Nelson:graphics:tooManyFunctions');
asserts.throws('fplot(@sin, ''BadName'', 1)', 'BadName', 'Nelson:graphics:invalidParameter');
asserts.throws('fplot(@sin, ''badstyle'')', 'badstyle', 'Nelson:graphics:invalidParameter');
%=============================================================================
close all
%=============================================================================
function y = scalarOnly(x)
  if numel(x) ~= 1
    error('scalar');
  end
  y = x + 1;
end
%=============================================================================
