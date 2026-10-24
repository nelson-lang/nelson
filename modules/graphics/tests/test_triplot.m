%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
P = [0 0; 1 0; 1 1; 0 1];
T = delaunay(P);
f = figure('Visible', 'off');
h = triplot(T, P(:, 1), P(:, 2));
asserts.istrue(isgraphics(h, 'line'));
asserts.isapprox(h.Color, [0.0660 0.4430 0.7450], 1e-4);
%=============================================================================
h = triplot(T, P(:, 1), P(:, 2), '--r', 'LineWidth', 2);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineWidth, 2);
%=============================================================================
h = triplot(T, P(:, 1), P(:, 2), 'or');
asserts.isequal(h.LineStyle, 'none');
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [1 0 0]);
%=============================================================================
DT = delaunayTriangulation(P);
h = triplot(DT);
asserts.istrue(isgraphics(h, 'line'));
%=============================================================================
P = [0 0 0; 1 0 0; 0 1 1; 1 1 2];
T = [1 2 3; 2 4 3];
TR = triangulation(T, P);
figure('Visible', 'off');
h = triplot(TR);
asserts.istrue(isgraphics(h, 'line'));
asserts.isequal(h.ZData, zeros(1, 0));
%=============================================================================
rng default;
P = rand([30 2]);
DT = delaunayTriangulation(P);
IC = incenter(DT);
h = triplot(DT);
hold on
p = plot(IC(:, 1), IC(:, 2), '*r');
asserts.istrue(isgraphics(h, 'line'));
asserts.istrue(isgraphics(p, 'line'));
%=============================================================================
figure('Visible', 'off');
txt = evalc('triplot(DT)');
asserts.isfalse(contains(txt, 'ans ='));
%=============================================================================
