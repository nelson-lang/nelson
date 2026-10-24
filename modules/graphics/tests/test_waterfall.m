%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
f = figure();
[X, Y] = meshgrid(-3:.125:3);
Z = peaks(X, Y);
w = waterfall(X', Y', Z');
%=============================================================================
f = figure();
[X, Y] = meshgrid(-5:.5:5);
Z = Y .* sin(X) - X .* cos(Y);
p = waterfall(X, Y, Z);
%=============================================================================
f = figure();
Z = peaks();
w = waterfall(Z);
t = title("waterfall function");
ax = gca();
asserts.istrue(all(f.Position(3:4) > 0));
asserts.isequal(ax.XTick, 0:10:50);
% waterfall now builds a patch (one curtain polygon per row) so the row line
% stays continuous. The curtain patch carries a real 3D Z extent and takes the
% shared nice-number auto limits (6-interval budget), matching the reference
% Z axis: ZLim [-10 10] with ticks every 5.
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isequal(ax.ZTick, -10:5:10);
asserts.isequal(t.Units, 'data');
asserts.isapprox(t.Position(1), 37.0484, 1e-3);
asserts.isapprox(t.Position(3), 13.7130, 1e-3);
% The curtain reads as coloured ribbon lines above light curtains: the fill is
% white, the edges interpolate the per-vertex colours, and the two baseline
% vertices of every row are white so the vertical drops and the baseline fade
% to white instead of being boxed in a single bright edge colour.
asserts.isequal(w.FaceColor, [1 1 1]);
asserts.isequal(w.EdgeColor, 'interp');
n = size(Z, 2);
nv = n + 2;
fvc = w.FaceVertexCData;
asserts.isequal(fvc(nv - 1, :), [1 1 1]);
asserts.isequal(fvc(nv, :), [1 1 1]);
% The last data vertex of the first row keeps its colormap colour (not white).
asserts.isfalse(isequal(fvc(n, :), [1 1 1]));
%=============================================================================
